{-# LANGUAGE LambdaCase #-}
module Web.Scotty.Util
    ( lazyTextToStrictByteString
    , strictByteStringToLazyText
    , setContent
    , setHeaderWith
    , setStatus
    , mkResponse
    , replace
    , add
    , addIfNotPresent
    , socketDescription
    , readRequestBody
    ) where

import Network.Socket (SockAddr(..), Socket, getSocketName, socketPort)
import Network.Wai

import Control.Monad (when)
import Control.Exception (throw)

import Network.HTTP.Types

import qualified Data.ByteString as B
import qualified Data.Text as TP (pack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as ES
import qualified Data.Text.Encoding.Error as ES

import Web.Scotty.Internal.Types

lazyTextToStrictByteString :: TL.Text -> B.ByteString
lazyTextToStrictByteString = ES.encodeUtf8 . TL.toStrict

strictByteStringToLazyText :: B.ByteString -> TL.Text
strictByteStringToLazyText = TL.fromStrict . ES.decodeUtf8With ES.lenientDecode

setContent :: Content -> ScottyResponse -> ScottyResponse
setContent c sr = sr { srContent = c }

setHeaderWith :: ([(HeaderName, B.ByteString)] -> [(HeaderName, B.ByteString)]) -> ScottyResponse -> ScottyResponse
setHeaderWith f sr = sr { srHeaders = f (srHeaders sr) }

setStatus :: Status -> ScottyResponse -> ScottyResponse
setStatus s sr = sr { srStatus = s }

-- Note: we currently don't support responseRaw, which may be useful
-- for websockets. However, we always read the request body, which
-- is incompatible with responseRaw responses.
mkResponse :: ScottyResponse -> Response
mkResponse sr = case srContent sr of
                    ContentBuilder  b   -> responseBuilder s h b
                    ContentFile     f   -> responseFile s h f Nothing
                    ContentStream   str -> responseStream s h str
                    ContentResponse res -> res
    where s = srStatus sr
          h = srHeaders sr

-- Note: we assume headers are not sensitive to order here (RFC 2616 specifies they are not)
replace :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
replace k v = add k v . filter ((/= k) . fst)

add :: a -> b -> [(a,b)] -> [(a,b)]
add k v m = (k,v):m

addIfNotPresent :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
addIfNotPresent k v = go
    where go []         = [(k,v)]
          go l@((x,y):r)
            | x == k    = l
            | otherwise = (x,y) : go r

-- Assemble a description from the Socket's PortID.
socketDescription :: Socket -> IO String
socketDescription sock = do
  sockName <- getSocketName sock
  case sockName of
    SockAddrUnix u -> return $ "unix socket " ++ u
    _              -> fmap (\port -> "port " ++ show port) $ socketPort sock

-- | return request body or throw a 'RequestException' if request body too big
readRequestBody :: IO B.ByteString -- ^ body chunk reader
                -> ([B.ByteString] -> IO [B.ByteString])
                -> Maybe Kilobytes -- ^ max body size
                -> IO [B.ByteString]
readRequestBody rbody prefix maxSize = do
  b <- rbody
  if B.null b then
       prefix []
    else
      do
        checkBodyLength maxSize
        readRequestBody rbody (prefix . (b:)) maxSize
    where checkBodyLength :: Maybe Kilobytes ->  IO ()
          checkBodyLength = \case
            Just maxSize' -> do
              bodySoFar <- prefix []
              when (isBigger bodySoFar maxSize') readUntilEmpty
            Nothing -> return ()
          isBigger bodySoFar maxSize' = (B.length . B.concat $ bodySoFar) > maxSize' * 1024
          readUntilEmpty = do
            b <- rbody
            if B.null b
              then throw (RequestException (ES.encodeUtf8 . TP.pack $ "Request is too big Jim!") status413)
              else readUntilEmpty
