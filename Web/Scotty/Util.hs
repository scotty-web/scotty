{-# LANGUAGE LambdaCase #-}
module Web.Scotty.Util
    ( lazyTextToStrictByteString
    , strictByteStringToLazyText
    , mkResponse
    , replace
    , add
    , addIfNotPresent
    , socketDescription
    , readRequestBody
    -- * exceptions
    , catch
    , catches
    , catchesOptionally
    , catchAny
    , try
    , tryAny
    ) where

import Network.Socket (SockAddr(..), Socket, getSocketName, socketPort)
import Network.Wai

import Control.Monad (when)
import           Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import Control.Exception (Exception (..), SomeException (..), IOException, SomeAsyncException (..))
import qualified Control.Exception as EUnsafe (fromException, throw, throwIO, catch)
import Data.Maybe (maybeToList)

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
              then EUnsafe.throw (RequestException (ES.encodeUtf8 . TP.pack $ "Request is too big Jim!") status413)
              else readUntilEmpty


-- exceptions

catchesOptionally :: MonadUnliftIO m =>
                     m a
                  -> Maybe (Handler m a) -- ^ if present, this 'Handler' is tried first
                  -> Handler m a -> m a
catchesOptionally io mh h = io `catches` (maybeToList mh <> [h])

catches :: MonadUnliftIO m => m a -> [Handler m a] -> m a
catches io handlers = io `catch` catchesHandler handlers

catchesHandler :: MonadIO m => [Handler m a] -> SomeException -> m a
catchesHandler handlers e = foldr tryHandler (liftIO (EUnsafe.throwIO e)) handlers
    where tryHandler (Handler h) res
              = case EUnsafe.fromException e of
                Just e' -> h e'
                Nothing -> res

-- | (from 'unliftio') Catch a synchronous (but not asynchronous) exception and recover from it.
catch
  :: (MonadUnliftIO m, Exception e)
  => m a -- ^ action
  -> (e -> m a) -- ^ handler
  -> m a
catch f g = withRunInIO $ \run -> run f `EUnsafe.catch` \e ->
  if isSyncException e
    then run (g e)
    -- intentionally rethrowing an async exception synchronously,
    -- since we want to preserve async behavior
    else EUnsafe.throwIO e

-- | 'catch' specialized to catch all synchronous exceptions.
catchAny :: MonadUnliftIO m => m a -> (SomeException -> m a) -> m a
catchAny = catch

-- | (from 'safe-exceptions') Check if the given exception is synchronous
isSyncException :: Exception e => e -> Bool
isSyncException e =
    case EUnsafe.fromException (toException e) of
        Just (SomeAsyncException _) -> False
        Nothing -> True

try :: (MonadUnliftIO m, Exception e) => m a -> m (Either e a)
try f = catch (Right <$> f) (pure . Left)

-- | 'try' specialized to catch all synchronous exceptions.
tryAny :: MonadUnliftIO m => m a -> m (Either SomeException a)
tryAny = try
