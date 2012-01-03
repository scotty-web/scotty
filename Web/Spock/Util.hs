module Web.Spock.Util
    ( lazyTextToStrictByteString
    , strictByteStringToLazyText
    , setContent, setHeader, setStatus
    ) where

import Network.Wai

import Network.HTTP.Types

import Blaze.ByteString.Builder (Builder)
import Data.CaseInsensitive (CI)
import Data.Default
import Data.Monoid

import qualified Data.ByteString as B
import qualified Data.Text.Lazy as T
import qualified Data.Text.Encoding as ES

instance Default Response where
    def = ResponseBuilder status200 [] mempty

lazyTextToStrictByteString :: T.Text -> B.ByteString
lazyTextToStrictByteString = ES.encodeUtf8 . T.toStrict

strictByteStringToLazyText :: B.ByteString -> T.Text
strictByteStringToLazyText = T.fromStrict . ES.decodeUtf8

-- Note: ResponseEnumerator is about to go away in favor of ResponseSource

setContent :: Either Builder FilePath -> Response -> Response
setContent (Left b) (ResponseBuilder s h _)  = ResponseBuilder s h b
setContent (Left b) (ResponseFile s h _ _)   = ResponseBuilder s h b
setContent (Left b) (ResponseEnumerator _)   = ResponseBuilder status200 [] b
-- setContent (Left b) (ResponseSource s h _)   = ResponseBuilder s h b
setContent (Right f) (ResponseBuilder s h _) = ResponseFile s h f Nothing
setContent (Right f) (ResponseFile s h _ _)  = ResponseFile s h f Nothing
setContent (Right f) (ResponseEnumerator _)  = ResponseFile status200 [] f Nothing
-- setContent (Right f) (ResponseSource s h _)  = ResponseFile s h f Nothing

setHeader :: (CI Ascii, Ascii) -> Response -> Response
setHeader (k,v) (ResponseBuilder s h b) = ResponseBuilder s (update h k v) b
setHeader (k,v) (ResponseFile s h f fp) = ResponseFile s (update h k v) f fp
setHeader _     re                      = re
-- setHeader (k,v) (ResponseSource s h cs) = ResponseSource s (update h k v) cs

setStatus :: Status -> Response -> Response
setStatus s (ResponseBuilder _ h b) = ResponseBuilder s h b
setStatus s (ResponseFile _ h f fp) = ResponseFile s h f fp
setStatus _ re                      = re
-- setStatus s (ResponseSource _ h cs) = ResponseSource s h cs

-- Note: we assume headers are not sensitive to order here (RFC 2616 specifies they are not)
update :: (Eq a) => [(a,b)] -> a -> b -> [(a,b)]
update m k v = (k,v) : filter ((/= k) . fst) m
