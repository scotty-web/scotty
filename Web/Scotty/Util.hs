module Web.Scotty.Util
    ( lazyTextToStrictByteString
    , strictByteStringToLazyText
    , setContent
    , setHeaderWith
    , setStatus
    , Content(..)
    , replace
    , add
    ) where

import Network.Wai

import Network.HTTP.Types

import Blaze.ByteString.Builder (Builder)
import Data.Conduit (Flush, Source, ResourceT)
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

data Content = ContentBuilder Builder
             | ContentFile FilePath
             | ContentSource (Source (ResourceT IO) (Flush Builder))

setContent :: Content -> Response -> Response
setContent (ContentBuilder b)  (ResponseBuilder s h _) = ResponseBuilder s h b
setContent (ContentBuilder b)  (ResponseFile s h _ _)  = ResponseBuilder s h b
setContent (ContentBuilder b)  (ResponseSource s h _)  = ResponseBuilder s h b
setContent (ContentFile f)     (ResponseBuilder s h _) = ResponseFile s h f Nothing
setContent (ContentFile f)     (ResponseFile s h _ _)  = ResponseFile s h f Nothing
setContent (ContentFile f)     (ResponseSource s h _)  = ResponseFile s h f Nothing
setContent (ContentSource src) (ResponseBuilder s h _) = ResponseSource s h src
setContent (ContentSource src) (ResponseFile s h _ _)  = ResponseSource s h src
setContent (ContentSource src) (ResponseSource s h _)  = ResponseSource s h src

setHeaderWith :: ([(HeaderName, B.ByteString)] -> [(HeaderName, B.ByteString)]) -> Response -> Response
setHeaderWith g (ResponseBuilder s h b) = ResponseBuilder s (g h) b
setHeaderWith g (ResponseFile s h f fp) = ResponseFile s (g h) f fp
setHeaderWith g (ResponseSource s h cs) = ResponseSource s (g h) cs

setStatus :: Status -> Response -> Response
setStatus s (ResponseBuilder _ h b) = ResponseBuilder s h b
setStatus s (ResponseFile _ h f fp) = ResponseFile s h f fp
setStatus s (ResponseSource _ h cs) = ResponseSource s h cs

-- Note: we assume headers are not sensitive to order here (RFC 2616 specifies they are not)
replace :: (Eq a) => a -> b -> [(a,b)] -> [(a,b)]
replace k v m = add k v $ filter ((/= k) . fst) m

add :: (Eq a) => a -> b -> [(a,b)] -> [(a,b)]
add k v m = (k,v):m
