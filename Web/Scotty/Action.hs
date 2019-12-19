{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Web.Scotty.Action
    ( addHeader
    , body
    , bodyReader
    , file
    , files
    , finish
    , header
    , headers
    , html
    , liftAndCatchIO
    , json
    , jsonData
    , next
    , param
    , params
    , raise
    , raiseStatus
    , raw
    , readEither
    , redirect
    , request
    , rescue
    , setHeader
    , status
    , stream
    , text
    , Param
    , Parsable(..)
      -- private to Scotty
    , runAction
    ) where

import           Blaze.ByteString.Builder   (fromLazyByteString)

import qualified Control.Exception          as E
import           Control.Monad.Error.Class
import           Control.Monad.Reader       hiding (mapM)
import qualified Control.Monad.State.Strict as MS
import           Control.Monad.Trans.Except

import qualified Data.Aeson                 as A
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.CaseInsensitive       as CI
import           Data.Default.Class         (def)
import           Data.Int
import qualified Data.Text                  as ST
import qualified Data.Text.Encoding         as STE
import qualified Data.Text.Lazy             as T
import           Data.Text.Lazy.Encoding    (encodeUtf8)
import           Data.Word

import           Network.HTTP.Types
-- not re-exported until version 0.11
#if !MIN_VERSION_http_types(0,11,0)
import           Network.HTTP.Types.Status
#endif
import           Network.Wai

import           Numeric.Natural

import           Prelude ()
import           Prelude.Compat

import           Web.Scotty.Internal.Types
import           Web.Scotty.Util

-- Nothing indicates route failed (due to Next) and pattern matching should continue.
-- Just indicates a successful response.
runAction :: (ScottyError e, Monad m) => ErrorHandler e m -> ActionEnv -> ActionT e m () -> m (Maybe Response)
runAction h env action = do
    (e,r) <- flip MS.runStateT def
           $ flip runReaderT env
           $ runExceptT
           $ runAM
           $ action `catchError` (defH h)
    return $ either (const Nothing) (const $ Just $ mkResponse r) e

-- | Default error handler for all actions.
defH :: (ScottyError e, Monad m) => ErrorHandler e m -> ActionError e -> ActionT e m ()
defH _          (Redirect url)    = do
    status status302
    setHeader "Location" url
defH Nothing    (ActionError s e)   = do
    status s
    let code = T.pack $ show $ statusCode s
    let msg = T.fromStrict $ STE.decodeUtf8 $ statusMessage s
    html $ mconcat ["<h1>", code, " ", msg, "</h1>", showError e]
defH h@(Just f) (ActionError _ e)   = f e `catchError` (defH h) -- so handlers can throw exceptions themselves
defH _          Next              = next
defH _          Finish            = return ()

-- | Throw an exception, which can be caught with 'rescue'. Uncaught exceptions
-- turn into HTTP 500 responses.
raise :: (ScottyError e, Monad m) => e -> ActionT e m a
raise = raiseStatus status500

-- | Throw an exception, which can be caught with 'rescue'. Uncaught exceptions turn into HTTP responses corresponding to the given status.
raiseStatus :: (ScottyError e, Monad m) => Status -> e -> ActionT e m a
raiseStatus s = throwError . ActionError s

-- | Abort execution of this action and continue pattern matching routes.
-- Like an exception, any code after 'next' is not executed.
--
-- As an example, these two routes overlap. The only way the second one will
-- ever run is if the first one calls 'next'.
--
-- > get "/foo/:bar" $ do
-- >   w :: Text <- param "bar"
-- >   unless (w == "special") next
-- >   text "You made a request to /foo/special"
-- >
-- > get "/foo/:baz" $ do
-- >   w <- param "baz"
-- >   text $ "You made a request to: " <> w
next :: (ScottyError e, Monad m) => ActionT e m a
next = throwError Next

-- | Catch an exception thrown by 'raise'.
--
-- > raise "just kidding" `rescue` (\msg -> text msg)
rescue :: (ScottyError e, Monad m) => ActionT e m a -> (e -> ActionT e m a) -> ActionT e m a
rescue action h = catchError action $ \e -> case e of
    ActionError _ err -> h err            -- handle errors
    other             -> throwError other -- rethrow internal error types

-- | Like 'liftIO', but catch any IO exceptions and turn them into 'ScottyError's.
liftAndCatchIO :: (ScottyError e, MonadIO m) => IO a -> ActionT e m a
liftAndCatchIO io = ActionT $ do
    r <- liftIO $ liftM Right io `E.catch` (\ e -> return $ Left $ stringError $ show (e :: E.SomeException))
    either throwError return r

-- | Redirect to given URL. Like throwing an uncatchable exception. Any code after the call to redirect
-- will not be run.
--
-- > redirect "http://www.google.com"
--
-- OR
--
-- > redirect "/foo/bar"
redirect :: (ScottyError e, Monad m) => T.Text -> ActionT e m a
redirect = throwError . Redirect

-- | Finish the execution of the current action. Like throwing an uncatchable
-- exception. Any code after the call to finish will not be run.
--
-- /Since: 0.10.3/
finish :: (ScottyError e, Monad m) => ActionT e m a
finish = throwError Finish

-- | Get the 'Request' object.
request :: Monad m => ActionT e m Request
request = ActionT $ liftM getReq ask

-- | Get list of uploaded files.
files :: Monad m => ActionT e m [File]
files = ActionT $ liftM getFiles ask

-- | Get a request header. Header name is case-insensitive.
header :: (ScottyError e, Monad m) => T.Text -> ActionT e m (Maybe T.Text)
header k = do
    hs <- liftM requestHeaders request
    return $ fmap strictByteStringToLazyText $ lookup (CI.mk (lazyTextToStrictByteString k)) hs

-- | Get all the request headers. Header names are case-insensitive.
headers :: (ScottyError e, Monad m) => ActionT e m [(T.Text, T.Text)]
headers = do
    hs <- liftM requestHeaders request
    return [ ( strictByteStringToLazyText (CI.original k)
             , strictByteStringToLazyText v)
           | (k,v) <- hs ]

-- | Get the request body.
body :: (ScottyError e,  MonadIO m) => ActionT e m BL.ByteString
body = ActionT ask >>= (liftIO . getBody)

-- | Get an IO action that reads body chunks
--
-- * This is incompatible with 'body' since 'body' consumes all chunks.
bodyReader :: Monad m => ActionT e m (IO B.ByteString)
bodyReader = ActionT $ getBodyChunk `liftM` ask

-- | Parse the request body as a JSON object and return it.
--
--   If the JSON object is malformed, this sets the status to
--   400 Bad Request, and throws an exception.
--
--   If the JSON fails to parse, this sets the status to
--   422 Unprocessable Entity.
--
--   These status codes are as per https://www.restapitutorial.com/httpstatuscodes.html.
jsonData :: (A.FromJSON a, ScottyError e, MonadIO m) => ActionT e m a
jsonData = do
    b <- body
    when (b == "") $ do
      let htmlError = "jsonData - No data was provided."
      raiseStatus status400 $ stringError htmlError
    case A.eitherDecode b of
      Left err -> do
        let htmlError = "jsonData - malformed."
              `mappend` " Data was: " `mappend` BL.unpack b
              `mappend` " Error was: " `mappend` err
        raiseStatus status400 $ stringError htmlError
      Right value -> case A.fromJSON value of
        A.Error err -> do
          let htmlError = "jsonData - failed parse."
                `mappend` " Data was: " `mappend` BL.unpack b `mappend` "."
                `mappend` " Error was: " `mappend` err
          raiseStatus status422 $ stringError htmlError
        A.Success a -> do
          return a

-- | Get a parameter. First looks in captures, then form data, then query parameters.
--
-- * Raises an exception which can be caught by 'rescue' if parameter is not found.
--
-- * If parameter is found, but 'read' fails to parse to the correct type, 'next' is called.
--   This means captures are somewhat typed, in that a route won't match if a correctly typed
--   capture cannot be parsed.
param :: (Parsable a, ScottyError e, Monad m) => T.Text -> ActionT e m a
param k = do
    val <- ActionT $ liftM (lookup k . getParams) ask
    case val of
        Nothing -> raise $ stringError $ "Param: " ++ T.unpack k ++ " not found!"
        Just v  -> either (const next) return $ parseParam v

-- | Get all parameters from capture, form and query (in that order).
params :: Monad m => ActionT e m [Param]
params = ActionT $ liftM getParams ask

-- | Minimum implemention: 'parseParam'
class Parsable a where
    -- | Take a 'T.Text' value and parse it as 'a', or fail with a message.
    parseParam :: T.Text -> Either T.Text a

    -- | Default implementation parses comma-delimited lists.
    --
    -- > parseParamList t = mapM parseParam (T.split (== ',') t)
    parseParamList :: T.Text -> Either T.Text [a]
    parseParamList t = mapM parseParam (T.split (== ',') t)

-- No point using 'read' for Text, ByteString, Char, and String.
instance Parsable T.Text where parseParam = Right
instance Parsable ST.Text where parseParam = Right . T.toStrict
instance Parsable B.ByteString where parseParam = Right . lazyTextToStrictByteString
instance Parsable BL.ByteString where parseParam = Right . encodeUtf8
-- | Overrides default 'parseParamList' to parse String.
instance Parsable Char where
    parseParam t = case T.unpack t of
                    [c] -> Right c
                    _   -> Left "parseParam Char: no parse"
    parseParamList = Right . T.unpack -- String
-- | Checks if parameter is present and is null-valued, not a literal '()'.
-- If the URI requested is: '/foo?bar=()&baz' then 'baz' will parse as (), where 'bar' will not.
instance Parsable () where
    parseParam t = if T.null t then Right () else Left "parseParam Unit: no parse"

instance (Parsable a) => Parsable [a] where parseParam = parseParamList

instance Parsable Bool where
    parseParam t = if t' == T.toCaseFold "true"
                   then Right True
                   else if t' == T.toCaseFold "false"
                        then Right False
                        else Left "parseParam Bool: no parse"
        where t' = T.toCaseFold t

instance Parsable Double where parseParam = readEither
instance Parsable Float where parseParam = readEither

instance Parsable Int where parseParam = readEither
instance Parsable Int8 where parseParam = readEither
instance Parsable Int16 where parseParam = readEither
instance Parsable Int32 where parseParam = readEither
instance Parsable Int64 where parseParam = readEither
instance Parsable Integer where parseParam = readEither

instance Parsable Word where parseParam = readEither
instance Parsable Word8 where parseParam = readEither
instance Parsable Word16 where parseParam = readEither
instance Parsable Word32 where parseParam = readEither
instance Parsable Word64 where parseParam = readEither
instance Parsable Natural where parseParam = readEither

-- | Useful for creating 'Parsable' instances for things that already implement 'Read'. Ex:
--
-- > instance Parsable Int where parseParam = readEither
readEither :: Read a => T.Text -> Either T.Text a
readEither t = case [ x | (x,"") <- reads (T.unpack t) ] of
                [x] -> Right x
                []  -> Left "readEither: no parse"
                _   -> Left "readEither: ambiguous parse"

-- | Set the HTTP response status. Default is 200.
status :: Monad m => Status -> ActionT e m ()
status = ActionT . MS.modify . setStatus

-- Not exported, but useful in the functions below.
changeHeader :: Monad m
             => (CI.CI B.ByteString -> B.ByteString -> [(HeaderName, B.ByteString)] -> [(HeaderName, B.ByteString)])
             -> T.Text -> T.Text -> ActionT e m ()
changeHeader f k = ActionT
                 . MS.modify
                 . setHeaderWith
                 . f (CI.mk $ lazyTextToStrictByteString k)
                 . lazyTextToStrictByteString

-- | Add to the response headers. Header names are case-insensitive.
addHeader :: Monad m => T.Text -> T.Text -> ActionT e m ()
addHeader = changeHeader add

-- | Set one of the response headers. Will override any previously set value for that header.
-- Header names are case-insensitive.
setHeader :: Monad m => T.Text -> T.Text -> ActionT e m ()
setHeader = changeHeader replace

-- | Set the body of the response to the given 'T.Text' value. Also sets \"Content-Type\"
-- header to \"text/plain; charset=utf-8\" if it has not already been set.
text :: (ScottyError e, Monad m) => T.Text -> ActionT e m ()
text t = do
    changeHeader addIfNotPresent "Content-Type" "text/plain; charset=utf-8"
    raw $ encodeUtf8 t

-- | Set the body of the response to the given 'T.Text' value. Also sets \"Content-Type\"
-- header to \"text/html; charset=utf-8\" if it has not already been set.
html :: (ScottyError e, Monad m) => T.Text -> ActionT e m ()
html t = do
    changeHeader addIfNotPresent "Content-Type" "text/html; charset=utf-8"
    raw $ encodeUtf8 t

-- | Send a file as the response. Doesn't set the \"Content-Type\" header, so you probably
-- want to do that on your own with 'setHeader'.
file :: Monad m => FilePath -> ActionT e m ()
file = ActionT . MS.modify . setContent . ContentFile

-- | Set the body of the response to the JSON encoding of the given value. Also sets \"Content-Type\"
-- header to \"application/json; charset=utf-8\" if it has not already been set.
json :: (A.ToJSON a, ScottyError e, Monad m) => a -> ActionT e m ()
json v = do
    changeHeader addIfNotPresent "Content-Type" "application/json; charset=utf-8"
    raw $ A.encode v

-- | Set the body of the response to a Source. Doesn't set the
-- \"Content-Type\" header, so you probably want to do that on your
-- own with 'setHeader'.
stream :: Monad m => StreamingBody -> ActionT e m ()
stream = ActionT . MS.modify . setContent . ContentStream

-- | Set the body of the response to the given 'BL.ByteString' value. Doesn't set the
-- \"Content-Type\" header, so you probably want to do that on your
-- own with 'setHeader'.
raw :: Monad m => BL.ByteString -> ActionT e m ()
raw = ActionT . MS.modify . setContent . ContentBuilder . fromLazyByteString
