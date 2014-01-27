{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module Web.Scotty.Action
    ( addHeader
    , body
    , file
    , files
    , html
    , json
    , jsonData
    , next
    , param
    , params
    , raise
    , raw
    , readEither
    , redirect
    , reqHeader
    , request
    , rescue
    , setHeader
    , source
    , status
    , text
    , Param
    , Parsable(..)
      -- private to Scotty
    , runAction
    ) where

import Blaze.ByteString.Builder (Builder, fromLazyByteString)

import Control.Monad.Error
import Control.Monad.Reader
import qualified Control.Monad.State as MS

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.CaseInsensitive as CI
import Data.Conduit (Flush, Source)
import Data.Default (def)
import Data.Monoid (mconcat)
import qualified Data.Text as ST
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (encodeUtf8)

import Network.HTTP.Types
import Network.Wai

import Web.Scotty.Types
import Web.Scotty.Util

-- Nothing indicates route failed (due to Next) and pattern matching should continue.
-- Just indicates a successful response.
runAction :: (ScottyError e, Monad m) => ErrorHandler e m -> ActionEnv -> ActionT e m () -> m (Maybe Response)
runAction h env action = do
    (e,r) <- flip MS.runStateT def
           $ flip runReaderT env
           $ runErrorT
           $ runAM
           $ action `catchError` (defH h)
    return $ either (const Nothing) (const $ Just $ mkResponse r) e

-- | Default error handler for all actions.
defH :: (ScottyError e, Monad m) => ErrorHandler e m -> ActionError e -> ActionT e m ()
defH _          (Redirect url)    = do
    status status302
    setHeader "Location" url
defH Nothing    (ActionError e)   = do
    status status500
    html $ mconcat ["<h1>500 Internal Server Error</h1>", showError e]
defH h@(Just f) (ActionError e)   = f e `catchError` (defH h) -- so handlers can throw exceptions themselves
defH _          Next              = next

-- | Throw an exception, which can be caught with 'rescue'. Uncaught exceptions
-- turn into HTTP 500 responses.
raise :: (ScottyError e, Monad m) => e -> ActionT e m a
raise = throwError . ActionError

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
    ActionError err -> h err            -- handle errors
    other           -> throwError other -- rethrow internal error types

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

-- | Get the 'Request' object.
request :: (ScottyError e, Monad m) => ActionT e m Request
request = ActionT $ liftM getReq ask

-- | Get list of uploaded files.
files :: (ScottyError e, Monad m) => ActionT e m [File]
files = ActionT $ liftM getFiles ask

-- | Get a request header. Header name is case-insensitive.
reqHeader :: (ScottyError e, Monad m) => T.Text -> ActionT e m (Maybe T.Text)
reqHeader k = do
    hs <- liftM requestHeaders request
    return $ fmap strictByteStringToLazyText $ lookup (CI.mk (lazyTextToStrictByteString k)) hs

-- | Get the request body.
body :: (ScottyError e, Monad m) => ActionT e m BL.ByteString
body = ActionT $ liftM getBody ask

-- | Parse the request body as a JSON object and return it. Raises an exception if parse is unsuccessful.
jsonData :: (A.FromJSON a, ScottyError e, Monad m) => ActionT e m a
jsonData = do
    b <- body
    maybe (raise $ stringError $ "jsonData - no parse: " ++ BL.unpack b) return $ A.decode b

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
params :: (ScottyError e, Monad m) => ActionT e m [Param]
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
    parseParam s
      | "true" `T.isPrefixOf` s'  = Right True
      | "false" `T.isPrefixOf` s' = Right False
      | otherwise                 = Left "parseParam Bool: no parse"
      where s' = T.toLower s

instance Parsable Double where parseParam = readEither
instance Parsable Float where parseParam = readEither
instance Parsable Int where parseParam = readEither
instance Parsable Integer where parseParam = readEither

-- | Useful for creating 'Parsable' instances for things that already implement 'Read'. Ex:
--
-- > instance Parsable Int where parseParam = readEither
readEither :: (Read a) => T.Text -> Either T.Text a
readEither t = case [ x | (x,"") <- reads (T.unpack t) ] of
                [x] -> Right x
                []  -> Left "readEither: no parse"
                _   -> Left "readEither: ambiguous parse"

-- | Set the HTTP response status. Default is 200.
status :: (ScottyError e, Monad m) => Status -> ActionT e m ()
status = ActionT . MS.modify . setStatus

-- | Add to the response headers. Header names are case-insensitive.
addHeader :: (ScottyError e, Monad m) => T.Text -> T.Text -> ActionT e m ()
addHeader k v = ActionT . MS.modify $ setHeaderWith $ add (CI.mk $ lazyTextToStrictByteString k) (lazyTextToStrictByteString v)

-- | Set one of the response headers. Will override any previously set value for that header.
-- Header names are case-insensitive.
setHeader :: (ScottyError e, Monad m) => T.Text -> T.Text -> ActionT e m ()
setHeader k v = ActionT . MS.modify $ setHeaderWith $ replace (CI.mk $ lazyTextToStrictByteString k) (lazyTextToStrictByteString v)

-- | Set the body of the response to the given 'T.Text' value. Also sets \"Content-Type\"
-- header to \"text/plain\".
text :: (ScottyError e, Monad m) => T.Text -> ActionT e m ()
text t = do
    setHeader "Content-Type" "text/plain"
    raw $ encodeUtf8 t

-- | Set the body of the response to the given 'T.Text' value. Also sets \"Content-Type\"
-- header to \"text/html\".
html :: (ScottyError e, Monad m) => T.Text -> ActionT e m ()
html t = do
    setHeader "Content-Type" "text/html"
    raw $ encodeUtf8 t

-- | Send a file as the response. Doesn't set the \"Content-Type\" header, so you probably
-- want to do that on your own with 'setHeader'.
file :: (ScottyError e, Monad m) => FilePath -> ActionT e m ()
file = ActionT . MS.modify . setContent . ContentFile

-- | Set the body of the response to the JSON encoding of the given value. Also sets \"Content-Type\"
-- header to \"application/json\".
json :: (A.ToJSON a, ScottyError e, Monad m) => a -> ActionT e m ()
json v = do
    setHeader "Content-Type" "application/json"
    raw $ A.encode v

-- | Set the body of the response to a Source. Doesn't set the
-- \"Content-Type\" header, so you probably want to do that on your
-- own with 'setHeader'.
source :: (ScottyError e, Monad m) => Source IO (Flush Builder) -> ActionT e m ()
source = ActionT . MS.modify . setContent . ContentSource

-- | Set the body of the response to the given 'BL.ByteString' value. Doesn't set the
-- \"Content-Type\" header, so you probably want to do that on your
-- own with 'setHeader'.
raw :: (ScottyError e, Monad m) => BL.ByteString -> ActionT e m ()
raw = ActionT . MS.modify . setContent . ContentBuilder . fromLazyByteString
