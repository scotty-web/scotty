{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE LambdaCase #-}
{-# language ScopedTypeVariables #-}
module Web.Scotty.Action
    ( addHeader
    , body
    , bodyReader
    , file
    , rawResponse
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
    , captureParam
    , formParam
    , queryParam
    , params
    , captureParams
    , formParams
    , queryParams
    , raise
    , raiseStatus
    , throw
    , raw
    , nested
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
import           Control.Monad              (when)
import           Control.Monad.IO.Class     (MonadIO(..))
import UnliftIO (MonadUnliftIO(..))
import           Control.Monad.Reader       (MonadReader(..), ReaderT(..))

import           Control.Concurrent.MVar

import qualified Data.Aeson                 as A
import Data.Bool (bool)
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.CaseInsensitive       as CI
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
import           Network.Wai (Request, Response, StreamingBody, Application, requestHeaders)

import           Numeric.Natural

import           Prelude ()
import "base-compat-batteries" Prelude.Compat

import           Web.Scotty.Internal.Types
import           Web.Scotty.Util (mkResponse, addIfNotPresent, add, replace, lazyTextToStrictByteString, strictByteStringToLazyText)
import Web.Scotty.Exceptions (Handler(..), catch, catchesOptionally, tryAny)

import Network.Wai.Internal (ResponseReceived(..))

-- | Evaluate a route, catch all exceptions (user-defined ones, internal and all remaining, in this order)
--   and construct the 'Response'
--
-- 'Nothing' indicates route failed (due to Next) and pattern matching should try the next available route.
-- 'Just' indicates a successful response.
runAction :: MonadUnliftIO m =>
             Maybe (ErrorHandler m) -- ^ this handler (if present) is in charge of user-defined exceptions
          -> ActionEnv
          -> ActionT m () -- ^ Route action to be evaluated
          -> m (Maybe Response)
runAction mh env action = do
  let
    handlers = [
      statusErrorHandler, -- StatusError
      actionErrorHandler, -- ActionError i.e. Next, Finish, Redirect
      someExceptionHandler -- all remaining exceptions
               ]
  ok <- flip runReaderT env $ runAM $ tryNext (catchesOptionally action mh handlers )
  res <- getResponse env
  return $ bool Nothing (Just $ mkResponse res) ok

-- | Catches 'StatusError' and produces an appropriate HTTP response.
statusErrorHandler :: MonadIO m => ErrorHandler m
statusErrorHandler = Handler $ \case
  StatusError s e -> do
    status s
    let code = T.pack $ show $ statusCode s
    let msg = T.fromStrict $ STE.decodeUtf8 $ statusMessage s
    html $ mconcat ["<h1>", code, " ", msg, "</h1>", e]

-- | Exception handler in charge of 'ActionError'. Rethrowing 'Next' here is caught by 'tryNext'.
-- All other cases of 'ActionError' are converted to HTTP responses.
actionErrorHandler :: MonadIO m => ErrorHandler m
actionErrorHandler = Handler $ \case
  AERedirect url -> do
    status status302
    setHeader "Location" url
  AENext -> next
  AEFinish -> return ()

-- | Uncaught exceptions turn into HTTP 500 Server Error codes
someExceptionHandler :: MonadIO m => ErrorHandler m
someExceptionHandler = Handler $ \case
  (_ :: E.SomeException) -> status status500

-- | Throw a "500 Server Error" 'StatusError', which can be caught with 'rescue'.
--
-- Uncaught exceptions turn into HTTP 500 responses.
raise :: (MonadIO m) =>
         T.Text -- ^ Error text
      -> ActionT m a
raise  = raiseStatus status500

-- | Throw a 'StatusError' exception that has an associated HTTP error code and can be caught with 'rescue'.
--
-- Uncaught exceptions turn into HTTP responses corresponding to the given status.
raiseStatus :: Monad m => Status -> T.Text -> ActionT m a
raiseStatus s = E.throw . StatusError s

-- | Throw an exception which can be caught within the scope of the current Action with 'rescue' or 'catch'.
--
-- If the exception is not caught locally, another option is to implement a global 'Handler' (with 'defaultHandler') that defines its interpretation and a translation to HTTP error codes.
--
-- Uncaught exceptions turn into HTTP 500 responses.
throw :: (MonadIO m, E.Exception e) => e -> ActionT m a
throw = E.throw

-- | Abort execution of this action and continue pattern matching routes.
-- Like an exception, any code after 'next' is not executed.
--
-- NB : Internally, this is implemented with an exception that can only be
-- caught by the library, but not by the user.
--
-- As an example, these two routes overlap. The only way the second one will
-- ever run is if the first one calls 'next'.
--
-- > get "/foo/:bar" $ do
-- >   w :: Text <- captureParam "bar"
-- >   unless (w == "special") next
-- >   text "You made a request to /foo/special"
-- >
-- > get "/foo/:baz" $ do
-- >   w <- captureParam "baz"
-- >   text $ "You made a request to: " <> w
next :: Monad m => ActionT m a
next = E.throw AENext

-- | Catch an exception e.g. a 'StatusError' or a user-defined exception.
--
-- > raise JustKidding `rescue` (\msg -> text msg)
rescue :: (MonadUnliftIO m, E.Exception e) => ActionT m a -> (e -> ActionT m a) -> ActionT m a
rescue = catch

-- | Catch any synchronous IO exceptions
liftAndCatchIO :: MonadIO m => IO a -> ActionT m a
liftAndCatchIO io = liftIO $ do
  r <- tryAny io
  either E.throwIO pure r


-- | Redirect to given URL. Like throwing an uncatchable exception. Any code after the call to redirect
-- will not be run.
--
-- > redirect "http://www.google.com"
--
-- OR
--
-- > redirect "/foo/bar"
redirect :: (Monad m) => T.Text -> ActionT m a
redirect = E.throw . AERedirect

-- | Finish the execution of the current action. Like throwing an uncatchable
-- exception. Any code after the call to finish will not be run.
--
-- /Since: 0.10.3/
finish :: (Monad m) => ActionT m a
finish = E.throw AEFinish

-- | Get the 'Request' object.
request :: Monad m => ActionT m Request
request = ActionT $ envReq <$> ask

-- | Get list of uploaded files.
files :: Monad m => ActionT m [File]
files = ActionT $ envFiles <$> ask

-- | Get a request header. Header name is case-insensitive.
header :: (Monad m) => T.Text -> ActionT m (Maybe T.Text)
header k = do
    hs <- requestHeaders <$> request
    return $ fmap strictByteStringToLazyText $ lookup (CI.mk (lazyTextToStrictByteString k)) hs

-- | Get all the request headers. Header names are case-insensitive.
headers :: (Monad m) => ActionT m [(T.Text, T.Text)]
headers = do
    hs <- requestHeaders <$> request
    return [ ( strictByteStringToLazyText (CI.original k)
             , strictByteStringToLazyText v)
           | (k,v) <- hs ]

-- | Get the request body.
body :: (MonadIO m) => ActionT m BL.ByteString
body = ActionT ask >>= (liftIO . envBody)

-- | Get an IO action that reads body chunks
--
-- * This is incompatible with 'body' since 'body' consumes all chunks.
bodyReader :: Monad m => ActionT m (IO B.ByteString)
bodyReader = ActionT $ envBodyChunk <$> ask

-- | Parse the request body as a JSON object and return it.
--
--   If the JSON object is malformed, this sets the status to
--   400 Bad Request, and throws an exception.
--
--   If the JSON fails to parse, this sets the status to
--   422 Unprocessable Entity.
--
--   These status codes are as per https://www.restapitutorial.com/httpstatuscodes.html.
jsonData :: (A.FromJSON a, MonadIO m) => ActionT m a
jsonData = do
    b <- body
    when (b == "") $ do
      let htmlError = "jsonData - No data was provided."
      raiseStatus status400 $ T.pack htmlError
    case A.eitherDecode b of
      Left err -> do
        let htmlError = "jsonData - malformed."
              `mappend` " Data was: " `mappend` BL.unpack b
              `mappend` " Error was: " `mappend` err
        raiseStatus status400 $ T.pack htmlError
      Right value -> case A.fromJSON value of
        A.Error err -> do
          let htmlError = "jsonData - failed parse."
                `mappend` " Data was: " `mappend` BL.unpack b `mappend` "."
                `mappend` " Error was: " `mappend` err
          raiseStatus status422 $ T.pack htmlError
        A.Success a -> do
          return a

-- | Get a parameter. First looks in captures, then form data, then query parameters.
--
-- * Raises an exception which can be caught by 'rescue' if parameter is not found.
--
-- * If parameter is found, but 'parseParam' fails to parse to the correct type, 'next' is called.
--   This means captures are somewhat typed, in that a route won't match if a correctly typed
--   capture cannot be parsed.
param :: (Parsable a, MonadIO m) => T.Text -> ActionT m a
param k = do
    val <- ActionT $ (lookup k . getParams) <$> ask
    case val of
        Nothing -> raiseStatus status500 $ "Param: " <> k <> " not found!" -- FIXME
        Just v  -> either (const next) return $ parseParam v
{-# DEPRECATED param "(#204) Not a good idea to treat all parameters identically. Use captureParam, formParam and queryParam instead. "#-}

-- | Get a capture parameter.
--
-- * Raises an exception which can be caught by 'rescue' if parameter is not found. If the exception is not caught, scotty will return a HTTP error code 500 ("Internal Server Error") to the client.
--
-- * If the parameter is found, but 'parseParam' fails to parse to the correct type, 'next' is called.
captureParam :: (Parsable a, Monad m) => T.Text -> ActionT m a
captureParam = paramWith CaptureParam envCaptureParams status500

-- | Get a form parameter.
--
-- * Raises an exception which can be caught by 'rescue' if parameter is not found. If the exception is not caught, scotty will return a HTTP error code 400 ("Bad Request") to the client.
--
-- * This function raises a code 400 also if the parameter is found, but 'parseParam' fails to parse to the correct type.
formParam :: (Parsable a, Monad m) => T.Text -> ActionT m a
formParam = paramWith FormParam envFormParams status400

-- | Get a query parameter.
--
-- * Raises an exception which can be caught by 'rescue' if parameter is not found. If the exception is not caught, scotty will return a HTTP error code 400 ("Bad Request") to the client.
--
-- * This function raises a code 400 also if the parameter is found, but 'parseParam' fails to parse to the correct type.
queryParam :: (Parsable a, Monad m) => T.Text -> ActionT m a
queryParam = paramWith QueryParam envQueryParams status400

data ParamType = CaptureParam
               | FormParam
               | QueryParam
instance Show ParamType where
  show = \case
    CaptureParam -> "capture"
    FormParam -> "form"
    QueryParam -> "query"

paramWith :: (Monad m, Parsable b) =>
             ParamType
          -> (ActionEnv -> [Param])
          -> Status -- ^ HTTP status to return if parameter is not found
          -> T.Text -- ^ parameter name
          -> ActionT m b
paramWith ty f err k = do
    val <- ActionT $ (lookup k . f) <$> ask
    case val of
      Nothing -> raiseStatus err (T.unwords [T.pack (show ty), "parameter:", k, "not found!"])
      Just v ->
        let handleParseError = \case
              CaptureParam -> next
              _ -> raiseStatus err (T.unwords ["Cannot parse", v, "as a", T.pack (show ty), "parameter"])
        in either (const $ handleParseError ty) return $ parseParam v

-- | Get all parameters from capture, form and query (in that order).
params :: Monad m => ActionT m [Param]
params = paramsWith getParams
{-# DEPRECATED params "(#204) Not a good idea to treat all parameters identically. Use captureParams, formParams and queryParams instead. "#-}

-- | Get capture parameters
captureParams :: Monad m => ActionT m [Param]
captureParams = paramsWith envCaptureParams
-- | Get form parameters
formParams :: Monad m => ActionT m [Param]
formParams = paramsWith envFormParams
-- | Get query parameters
queryParams :: Monad m => ActionT m [Param]
queryParams = paramsWith envQueryParams

paramsWith :: Monad m => (ActionEnv -> a) -> ActionT m a
paramsWith f = ActionT (f <$> ask)

{-# DEPRECATED getParams "(#204) Not a good idea to treat all parameters identically" #-}
getParams :: ActionEnv -> [Param]
getParams e = envCaptureParams e <> envFormParams e <> envQueryParams e

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

-- | Set the HTTP response status.
status :: MonadIO m => Status -> ActionT m ()
status = modifyResponse . setStatus

-- Not exported, but useful in the functions below.
changeHeader :: MonadIO m
             => (CI.CI B.ByteString -> B.ByteString -> [(HeaderName, B.ByteString)] -> [(HeaderName, B.ByteString)])
             -> T.Text -> T.Text -> ActionT m ()
changeHeader f k =
  modifyResponse . setHeaderWith . f (CI.mk $ lazyTextToStrictByteString k) . lazyTextToStrictByteString

-- | Add to the response headers. Header names are case-insensitive.
addHeader :: MonadIO m => T.Text -> T.Text -> ActionT m ()
addHeader = changeHeader add

-- | Set one of the response headers. Will override any previously set value for that header.
-- Header names are case-insensitive.
setHeader :: MonadIO m => T.Text -> T.Text -> ActionT m ()
setHeader = changeHeader replace

-- | Set the body of the response to the given 'T.Text' value. Also sets \"Content-Type\"
-- header to \"text/plain; charset=utf-8\" if it has not already been set.
text :: (MonadIO m) => T.Text -> ActionT m ()
text t = do
    changeHeader addIfNotPresent "Content-Type" "text/plain; charset=utf-8"
    raw $ encodeUtf8 t

-- | Set the body of the response to the given 'T.Text' value. Also sets \"Content-Type\"
-- header to \"text/html; charset=utf-8\" if it has not already been set.
html :: (MonadIO m) => T.Text -> ActionT m ()
html t = do
    changeHeader addIfNotPresent "Content-Type" "text/html; charset=utf-8"
    raw $ encodeUtf8 t

-- | Send a file as the response. Doesn't set the \"Content-Type\" header, so you probably
-- want to do that on your own with 'setHeader'. Setting a status code will have no effect
-- because Warp will overwrite that to 200 (see 'Network.Wai.Handler.Warp.Internal.sendResponse').
file :: MonadIO m => FilePath -> ActionT m ()
file = modifyResponse . setContent . ContentFile

rawResponse :: MonadIO m => Response -> ActionT m ()
rawResponse = modifyResponse . setContent . ContentResponse

-- | Set the body of the response to the JSON encoding of the given value. Also sets \"Content-Type\"
-- header to \"application/json; charset=utf-8\" if it has not already been set.
json :: (A.ToJSON a, MonadIO m) => a -> ActionT m ()
json v = do
    changeHeader addIfNotPresent "Content-Type" "application/json; charset=utf-8"
    raw $ A.encode v

-- | Set the body of the response to a Source. Doesn't set the
-- \"Content-Type\" header, so you probably want to do that on your
-- own with 'setHeader'.
stream :: MonadIO m => StreamingBody -> ActionT m ()
stream = modifyResponse . setContent . ContentStream

-- | Set the body of the response to the given 'BL.ByteString' value. Doesn't set the
-- \"Content-Type\" header, so you probably want to do that on your
-- own with 'setHeader'.
raw :: MonadIO m => BL.ByteString -> ActionT m ()
raw = modifyResponse . setContent . ContentBuilder . fromLazyByteString

-- | Nest a whole WAI application inside a Scotty handler.
-- See Web.Scotty for further documentation
nested :: (MonadIO m) => Network.Wai.Application -> ActionT m ()
nested app = do
  -- Is MVar really the best choice here? Not sure.
  r <- request
  ref <- liftIO $ newEmptyMVar
  _ <- liftAndCatchIO $ app r (\res -> putMVar ref res >> return ResponseReceived)
  res <- liftAndCatchIO $ readMVar ref
  rawResponse res
