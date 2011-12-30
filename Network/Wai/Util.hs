module Network.Wai.Util
    ( setStatus, setHeader
    ) where

import Network.Wai

import Network.HTTP.Types

import Data.CaseInsensitive (CI)
import Data.Default
import Data.Monoid

instance Default Response where
    def = ResponseBuilder status200 [] mempty

setStatus :: Status -> Response -> Response
setStatus s (ResponseBuilder _ h b) = ResponseBuilder s h b
setStatus s (ResponseFile _ h f fp) = ResponseFile s h f fp
-- setStatus s (ResponseSource _ h cs) = ResponseSource s h cs

setHeader :: (CI Ascii, Ascii) -> Response -> Response
setHeader (k,v) (ResponseBuilder s h b) = ResponseBuilder s (update h k v) b
setHeader (k,v) (ResponseFile s h f fp) = ResponseFile s (update h k v) f fp
-- setHeader (k,v) (ResponseSource s h cs) = ResponseSource s (update h k v) cs

-- Note: we assume headers are not sensitive to order here (RFC 2616 specifies they are not)
update :: (Eq a) => [(a,b)] -> a -> b -> [(a,b)]
update m k v = (k,v) : filter ((/= k) . fst) m

