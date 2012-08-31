{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Web.Scotty.Action
    ( request, files, reqHeader, body, param, params, jsonData
    , status, header, redirect
    , text, html, file, json, source
    , raise, rescue, next
    , ActionM, Parsable(..), readEither, Param, runAction
    ) where

import Blaze.ByteString.Builder (Builder, fromLazyByteString,fromByteString)

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import qualified Control.Monad.State as MS

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Lazy()
import qualified Data.CaseInsensitive as CI
import Data.Conduit (Flush, ResourceT, Source)
import Data.Default (Default, def)
import Data.Monoid (mconcat)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)

import Network.HTTP.Types
import Network.Wai

import Web.Scotty.Types
import Web.Scotty.Util




class ScottyString a where
  toContent :: a -> Content
  toText :: a -> T.Text
  fromScotty :: T.Text -> a

instance ScottyString B.ByteString where
  toContent bs = ContentBuilder (fromByteString bs)
  toText bs = toText $ BL.fromChunks [bs]
  fromScotty =  B.concat . BL.toChunks . fromScotty

instance ScottyString BL.ByteString where
  toContent bs   = ContentBuilder (fromLazyByteString bs)
  toText  = decodeUtf8
  fromScotty = encodeUtf8

instance ScottyString T.Text where
  toContent = toContent . encodeUtf8
  toText = id
  fromScotty = id

instance ScottyString String where
  toContent = toContent . B.pack
  toText = T.pack
  fromScotty = T.unpack

-- Nothing indicates route failed (due to Next) and pattern matching should continue.
-- Just indicates a successful response.
runAction :: ActionEnv -> ActionM () -> IO (Maybe Response)
runAction env action = do
    (e,r) <- flip MS.runStateT def
           $ flip runReaderT env
           $ runErrorT
           $ runAM
           $ action `catchError` defaultHandler
    return $ either (const Nothing) (const $ Just r) e

defaultHandler :: ActionError -> ActionM ()
defaultHandler (Redirect url) = do
    status status302
    header "Location" url
defaultHandler (ActionError msg) = do
    status status500
    html $ mconcat ["<h1>500 Internal Server Error</h1>", msg]
defaultHandler Next = next

-- | Throw an exception, which can be caught with 'rescue'. Uncaught exceptions
-- turn into HTTP 500 responses.
raise :: T.Text -> ActionM b
raise = throwError . ActionError . toText

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
next :: ActionM a
next = throwError Next

-- | Catch an exception thrown by 'raise'.
--
-- > raise "just kidding" `rescue` (\msg -> text msg)
rescue :: ActionM a -> (T.Text -> ActionM a) -> ActionM a
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
redirect :: ScottyString a => a -> ActionM a
redirect = throwError . Redirect . toText

-- | Get the 'Request' object.
request :: ActionM Request
request = getReq <$> ask

-- | Get list of uploaded files.
files :: ActionM [File]
files = getFiles <$> ask

-- | Get a request header. Header name is case-insensitive.
reqHeader :: (ScottyString a) => T.Text -> ActionM a
reqHeader k = do
    hs <- requestHeaders <$> request
    maybe (raise (mconcat ["reqHeader: ", k, " not found"]))
          (return . fromScotty . strictByteStringToLazyText)
          (lookup (CI.mk (lazyTextToStrictByteString $ toText k)) hs)

-- | Get the request body.
body :: ActionM BL.ByteString
body = getBody <$> ask

-- | Parse the request body as a JSON object and return it. Raises an exception if parse is unsuccessful.
jsonData :: (A.FromJSON a) => ActionM a
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
param :: (Parsable a) => T.Text -> ActionM a
param k = do
    val <- lookup k <$> getParams <$> ask
    case val of
        Nothing -> raise $ mconcat ["Param: ", k, " not found!"]
        Just v  -> either (const next) return $ parseParam v

-- | Get all parameters from capture, form and query (in that order).
params :: ActionM [Param]
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
status :: Status -> ActionM ()
status = MS.modify . setStatus

-- | Set one of the response headers. Will override any previously set value for that header.
-- Header names are case-insensitive.
header :: ScottyString a => T.Text -> a -> ActionM ()
header k v = MS.modify $ setHeader (CI.mk k', v')
  where k' = lazyTextToStrictByteString k
        v' = lazyTextToStrictByteString $ toText v

-- | Set the body of the response to the given 'a' value. Also sets \"Content-Type\"
-- header to \"text/plain\".
text :: (ScottyString a) => a -> ActionM ()
text t = do
    header "Content-Type" ("text/plain" ::T.Text)
    MS.modify $ setContent $ toContent t


-- | Set the body of the response to the given 'a' value. Also sets \"Content-Type\"
-- header to \"text/html\".
html :: (ScottyString a) => a -> ActionM ()
html t = do
    header "Content-Type" ("text/html" ::T.Text)
    MS.modify $ setContent $ toContent t

-- | Send a file as the response. Doesn't set the \"Content-Type\" header, so you probably
-- want to do that on your own with 'header'.
file :: FilePath -> ActionM ()
file = MS.modify . setContent . ContentFile

-- | Set the body of the response to the JSON encoding of the given value. Also sets \"Content-Type\"
-- header to \"application/json\".
json :: (A.ToJSON a) => a -> ActionM ()
json v = do
    header "Content-Type" ("application/json" ::T.Text)
    MS.modify $ setContent $ ContentBuilder $ fromLazyByteString $ A.encode v

-- | Set the body of the response to a Source. Doesn't set the
-- \"Content-Type\" header, so you probably want to do that on your
-- own with 'header'.
source :: Source (ResourceT IO) (Flush Builder) -> ActionM ()
source = MS.modify . setContent . ContentSource
