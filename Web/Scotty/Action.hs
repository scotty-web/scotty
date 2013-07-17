{-# LANGUAGE OverloadedStrings #-}
module Web.Scotty.Action
    ( request, files, reqHeader, body, param, params, jsonData
    , status, header, redirect
    , text, html, file, json, source, raw
    , raise, rescue, next
    , ActionM, Parsable(..), readEither, Param, runAction
    ) where

import Blaze.ByteString.Builder (Builder, fromLazyByteString)

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import qualified Control.Monad.State as MS

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.CaseInsensitive as CI
import Data.Conduit (Flush, ResourceT, Source)
import Data.Default (def)
import Data.Monoid (mconcat)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (encodeUtf8)

import Network.HTTP.Types
import Network.Wai

import Web.Scotty.Types
import Web.Scotty.Util

-- Nothing indicates route failed (due to Next) and pattern matching should continue.
-- Just indicates a successful response.
runAction :: (Monad m)
          => ActionEnv -> ActionT m () -> m (Maybe Response)
runAction env action = do
    (e,r) <- flip MS.runStateT def
           $ flip runReaderT env
           $ runErrorT
           $ runAT
           $ action `catchError` defaultHandler
    return $ either (const Nothing) (const $ Just r) e

defaultHandler :: (Monad m) => ActionError -> ActionT m ()
defaultHandler (Redirect url) = do
    status status302
    header "Location" url
defaultHandler (ActionError msg) = do
    status status500
    html $ mconcat ["<h1>500 Internal Server Error</h1>", msg]
defaultHandler Next = next

-- | Throw an exception, which can be caught with 'rescue'. Uncaught exceptions
-- turn into HTTP 500 responses.
raise :: (Monad m) => T.Text -> ActionT m a
raise = throwError . ActionError

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
next :: (Monad m) => ActionT m a
next = throwError Next

-- | Catch an exception thrown by 'raise'.
--
-- > raise "just kidding" `rescue` (\msg -> text msg)
rescue :: (Monad m) => ActionT m a -> (T.Text -> ActionT m a) -> ActionT m a
rescue action handler = catchError action $ \e -> case e of
    ActionError msg -> handler msg      -- handle errors
    other           -> throwError other -- rethrow redirects and nexts

-- | Redirect to given URL. Like throwing an uncatchable exception. Any code after the call to redirect
-- will not be run.
--
-- > redirect "http://www.google.com"
--
-- OR
--
-- > redirect "/foo/bar"
redirect :: Monad m => T.Text -> ActionT m a
redirect = throwError . Redirect

-- | Get the 'Request' object.
request :: (Functor m, Monad m) => ActionT m Request
request = getReq <$> ask

-- | Get list of uploaded files.
files :: (Functor m, Monad m) => ActionT m [File]
files = getFiles <$> ask

-- | Get a request header. Header name is case-insensitive.
reqHeader :: (Functor m, Monad m) => T.Text -> ActionT m T.Text
reqHeader k = do
    hs <- requestHeaders <$> request
    maybe (raise (mconcat ["reqHeader: ", k, " not found"]))
          (return . strictByteStringToLazyText)
          (lookup (CI.mk (lazyTextToStrictByteString k)) hs)

-- | Get the request body.
body :: (Functor m, Monad m) => ActionT m BL.ByteString
body = getBody <$> ask

-- | Parse the request body as a JSON object and return it. Raises an exception if parse is unsuccessful.
jsonData :: (A.FromJSON a, Functor m, Monad m) => ActionT m a
jsonData = do
    b <- body
    maybe (raise "jsonData: no parse") return $ A.decode b

-- | Get a parameter. First looks in captures, then form data, then query parameters.
--
-- * Raises an exception which can be caught by 'rescue' if parameter is not found.
--
-- * If parameter is found, but 'read' fails to parse to the correct type, 'next' is called.
--   This means captures are somewhat typed, in that a route won't match if a correctly typed
--   capture cannot be parsed.
param :: (Parsable a, Functor m, Monad m) => T.Text -> ActionT m a
param k = do
    val <- lookup k <$> getParams <$> ask
    case val of
        Nothing -> raise $ mconcat ["Param: ", k, " not found!"]
        Just v  -> either (const next) return $ parseParam v

-- | Get all parameters from capture, form and query (in that order).
params :: (Functor m, Monad m) => ActionT m [Param]
params = getParams <$> ask

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
status :: (Monad m) => Status -> ActionT m ()
status = MS.modify . setStatus

-- | Set one of the response headers. Will override any previously set value for that header.
-- Header names are case-insensitive.
header :: (Monad m) => T.Text -> T.Text -> ActionT m ()
header k v = MS.modify $ setHeader (CI.mk $ lazyTextToStrictByteString k, lazyTextToStrictByteString v)

-- | Set the body of the response to the given 'T.Text' value. Also sets \"Content-Type\"
-- header to \"text/plain\".
text :: (Monad m) => T.Text -> ActionT m ()
text t = do
    header "Content-Type" "text/plain"
    raw $ encodeUtf8 t

-- | Set the body of the response to the given 'T.Text' value. Also sets \"Content-Type\"
-- header to \"text/html\".
html :: (Monad m) => T.Text -> ActionT m ()
html t = do
    header "Content-Type" "text/html"
    raw $ encodeUtf8 t

-- | Send a file as the response. Doesn't set the \"Content-Type\" header, so you probably
-- want to do that on your own with 'header'.
file :: (Monad m) => FilePath -> ActionT m ()
file = MS.modify . setContent . ContentFile

-- | Set the body of the response to the JSON encoding of the given value. Also sets \"Content-Type\"
-- header to \"application/json\".
json :: (A.ToJSON a, Monad m) => a -> ActionT m ()
json v = do
    header "Content-Type" "application/json"
    raw $ A.encode v

-- | Set the body of the response to a Source. Doesn't set the
-- \"Content-Type\" header, so you probably want to do that on your
-- own with 'header'.
source :: Source (ResourceT IO) (Flush Builder) -> ActionM ()
source = MS.modify . setContent . ContentSource

-- | Set the body of the response to the given 'BL.ByteString' value. Doesn't set the
-- \"Content-Type\" header, so you probably want to do that on your
-- own with 'header'.
raw :: (Monad m) => BL.ByteString -> ActionT m ()
raw = MS.modify . setContent . ContentBuilder . fromLazyByteString
