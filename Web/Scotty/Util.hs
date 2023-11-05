{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
module Web.Scotty.Util
    ( lazyTextToStrictByteString
    , strictByteStringToLazyText
    , decodeUtf8Lenient
    , mkResponse
    , replace
    , add
    , addIfNotPresent
    , socketDescription
    , readRequestBody
    ) where

import Network.Socket (SockAddr(..), Socket, getSocketName, socketPort)
import Network.Wai

import Control.Exception
import Control.Monad (when)
import qualified Data.ByteString as B
import qualified Data.Text as TP (Text, pack)
import qualified Data.Text.Lazy as TL
import           Data.Text.Encoding as ES
import qualified Data.Text.Encoding.Error as ES

import Web.Scotty.Internal.Types

lazyTextToStrictByteString :: TL.Text -> B.ByteString
lazyTextToStrictByteString = ES.encodeUtf8 . TL.toStrict

strictByteStringToLazyText :: B.ByteString -> TL.Text
strictByteStringToLazyText = TL.fromStrict . ES.decodeUtf8With ES.lenientDecode

#if !MIN_VERSION_text(2,0,0)
decodeUtf8Lenient :: B.ByteString -> TP.Text
decodeUtf8Lenient = ES.decodeUtf8With ES.lenientDecode
#endif

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
              when (bodySoFar `isBigger` maxSize') readUntilEmpty
            Nothing -> return ()
          isBigger bodySoFar maxSize' = (B.length . B.concat $ bodySoFar) > maxSize' * 1024 -- XXX this looks both inefficient and wrong
          readUntilEmpty = do
            b <- rbody
            if B.null b
              then throwIO RequestTooLarge
              else readUntilEmpty



