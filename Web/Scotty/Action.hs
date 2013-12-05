{-# LANGUAGE OverloadedStrings #-}
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
import Data.Monoid (mconcat, (<>))
import qualified Data.Text as ST
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)

import Network.HTTP.Types
import Network.Wai

import Web.Scotty.Types
import Web.Scotty.Util

-- Nothing indicates route failed (due to Next) and pattern matching should continue.
-- Just indicates a successful response.
runAction :: Monad m => ActionEnv -> ActionT e m () -> m (Maybe Response)
runAction env action = do
    (e,r) <- flip MS.runStateT def
           $ flip runReaderT env
           $ runErrorT
           $ runAM
           $ action `catchError` defaultHandler
    return $ either (const Nothing) (const $ Just $ mkResponse r) e

defaultHandler :: Monad m => ActionError e -> ActionT e m ()
defaultHandler (Redirect url) = do
    status status302
    setHeader "Location" url
defaultHandler (StringError msg) = do
    status status500
    html $ mconcat ["<h1>500 Internal Server Error</h1>", msg]
defaultHandler Next = next
defaultHandler (ActionError _) = do
    status status500
    html $ mconcat ["<h1>500 Internal Server Error</h1>"
                   ,"<br/>Uncaught Custom Exception"]

-- | Throw an exception, which can be caught with 'rescue'. Uncaught exceptions
-- turn into HTTP 500 responses.
raise :: Monad m => T.Text -> ActionT e m a
raise = throwError . StringError

-- | Abort execution of this action and continue pattern matching routes.
-- Like an exception, any code after 'next' is not executed.
--
-- As an example, these two routes overlap. The only way the second one will
-- ever run is if the first one calls 'next'.
--
-- > get "/foo/:number" $ do
-- >   n <- param "number"
-- >   unless (all isDigit n) $ next
-- >   text "a number"
-- >
-- > get "/foo/:bar" $ do
-- >   bar <- param "bar"
-- >   text "not a number"
next :: Monad m => ActionT e m a
next = throwError Next

-- | Catch an exception thrown by 'raise'.
--
-- > raise "just kidding" `rescue` (\msg -> text msg)
rescue :: Monad m => ActionT e m a -> (T.Text -> ActionT e m a) -> ActionT e m a
rescue action handler = catchError action $ \e -> case e of
    StringError msg -> handler msg      -- handle errors
    other           -> throwError other -- rethrow redirects and nexts

-- | Redirect to given URL. Like throwing an uncatchable exception. Any code after the call to redirect
-- will not be run.
--
-- > redirect "http://www.google.com"
--
-- OR
--
-- > redirect "/foo/bar"
redirect :: Monad m => T.Text -> ActionT e m a
redirect = throwError . Redirect

-- | Get the 'Request' object.
request :: Monad m => ActionT e m Request
request = ActionT $ liftM getReq ask

-- | Get list of uploaded files.
files :: Monad m => ActionT e m [File]
files = ActionT $ liftM getFiles ask

-- | Get a request header. Header name is case-insensitive.
reqHeader :: Monad m => T.Text -> ActionT e m (Maybe T.Text)
reqHeader k = do
    hs <- liftM requestHeaders request
    return $ fmap strictByteStringToLazyText $ lookup (CI.mk (lazyTextToStrictByteString k)) hs

-- | Get the request body.
body :: Monad m => ActionT e m BL.ByteString
body = ActionT $ liftM getBody ask

-- | Parse the request body as a JSON object and return it. Raises an exception if parse is unsuccessful.
jsonData :: (A.FromJSON a, Monad m) => ActionT e m a
jsonData = do
    b <- body
    maybe (raise $ "jsonData - no parse: " <> decodeUtf8 b) return $ A.decode b

-- | Get a parameter. First looks in captures, then form data, then query parameters.
--
-- * Raises an exception which can be caught by 'rescue' if parameter is not found.
--
-- * If parameter is found, but 'read' fails to parse to the correct type, 'next' is called.
--   This means captures are somewhat typed, in that a route won't match if a correctly typed
--   capture cannot be parsed.
param :: (Parsable a, Monad m) => T.Text -> ActionT e m a
param k = do
    val <- ActionT $ liftM (lookup k . getParams) ask
    case val of
        Nothing -> raise $ mconcat ["Param: ", k, " not found!"]
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

instance Parsable Bool where parseParam = readEither
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
status :: Monad m => Status -> ActionT e m ()
status = ActionT . MS.modify . setStatus

-- | Add to the response headers. Header names are case-insensitive.
addHeader :: Monad m => T.Text -> T.Text -> ActionT e m ()
addHeader k v = ActionT . MS.modify $ setHeaderWith $ add (CI.mk $ lazyTextToStrictByteString k) (lazyTextToStrictByteString v)

-- | Set one of the response headers. Will override any previously set value for that header.
-- Header names are case-insensitive.
setHeader :: Monad m => T.Text -> T.Text -> ActionT e m ()
setHeader k v = ActionT . MS.modify $ setHeaderWith $ replace (CI.mk $ lazyTextToStrictByteString k) (lazyTextToStrictByteString v)

-- | Set the body of the response to the given 'T.Text' value. Also sets \"Content-Type\"
-- header to \"text/plain\".
text :: Monad m => T.Text -> ActionT e m ()
text t = do
    setHeader "Content-Type" "text/plain"
    raw $ encodeUtf8 t

-- | Set the body of the response to the given 'T.Text' value. Also sets \"Content-Type\"
-- header to \"text/html\".
html :: Monad m => T.Text -> ActionT e m ()
html t = do
    setHeader "Content-Type" "text/html"
    raw $ encodeUtf8 t

-- | Send a file as the response. Doesn't set the \"Content-Type\" header, so you probably
-- want to do that on your own with 'header'.
file :: Monad m => FilePath -> ActionT e m ()
file = ActionT . MS.modify . setContent . ContentFile

-- | Set the body of the response to the JSON encoding of the given value. Also sets \"Content-Type\"
-- header to \"application/json\".
json :: (A.ToJSON a, Monad m) => a -> ActionT e m ()
json v = do
    setHeader "Content-Type" "application/json"
    raw $ A.encode v

-- | Set the body of the response to a Source. Doesn't set the
-- \"Content-Type\" header, so you probably want to do that on your
-- own with 'header'.
source :: Monad m => Source IO (Flush Builder) -> ActionT e m ()
source = ActionT . MS.modify . setContent . ContentSource

-- | Set the body of the response to the given 'BL.ByteString' value. Doesn't set the
-- \"Content-Type\" header, so you probably want to do that on your
-- own with 'header'.
raw :: Monad m => BL.ByteString -> ActionT e m ()
raw = ActionT . MS.modify . setContent . ContentBuilder . fromLazyByteString
