{-# LANGUAGE FlexibleContexts, FlexibleInstances, RecordWildCards,
             OverloadedStrings, MultiWayIf #-}
module Web.Scotty.Body (
  newBodyInfo,
  cloneBodyInfo

  , getFormParamsAndFilesAction
  , getBodyAction
  , getBodyChunkAction
  -- wai-extra
  , RequestParseException(..)
  ) where

import           Control.Concurrent.MVar
import           Control.Monad.IO.Class
import           Control.Exception (catch)
import Control.Monad.Trans.Resource (InternalState)
import Data.Bifunctor (first, bimap)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Maybe
import qualified GHC.Exception as E (throw)
import           Network.Wai (Request(..), getRequestBodyChunk)
import qualified Network.Wai.Parse as W (File, Param, getRequestBodyType, BackEnd, lbsBackEnd, tempFileBackEnd, sinkRequestBody, RequestBodyType(..))
import           Web.Scotty.Action (Param)
import           Web.Scotty.Internal.Types (BodyInfo(..), BodyChunkBuffer(..), BodyPartiallyStreamed(..), RouteOptions(..), File)
import           Web.Scotty.Util (readRequestBody, strictByteStringToLazyText, decodeUtf8Lenient)

import Web.Scotty.Internal.WaiParseSafe (parseRequestBodyEx, defaultParseRequestBodyOptions, RequestParseException(..), ParseRequestBodyOptions(..))

-- | Make a new BodyInfo with readProgress at 0 and an empty BodyChunkBuffer.
newBodyInfo :: (MonadIO m) => Request -> m BodyInfo
newBodyInfo req = liftIO $ do
  readProgress <- newMVar 0
  chunkBuffer <- newMVar (BodyChunkBuffer False [])
  return $ BodyInfo readProgress chunkBuffer (getRequestBodyChunk req)

-- | Make a copy of a BodyInfo, sharing the previous BodyChunkBuffer but with the
-- readProgress MVar reset to 0.
cloneBodyInfo :: (MonadIO m) => BodyInfo -> m BodyInfo
cloneBodyInfo (BodyInfo _ chunkBufferVar getChunk) = liftIO $ do
  cleanReadProgressVar <- newMVar 0
  return $ BodyInfo cleanReadProgressVar chunkBufferVar getChunk

-- | Get the form params and files from the request.
-- Only reads the whole body if the request is URL-encoded
getFormParamsAndFilesAction :: InternalState -> ParseRequestBodyOptions -> Request -> BodyInfo -> RouteOptions -> IO ([Param], [File BL.ByteString], [File FilePath])
getFormParamsAndFilesAction istate prbo req bodyInfo opts = do
  let
    bs2t = decodeUtf8Lenient
    convertBoth = bimap bs2t bs2t
    convertKey = first bs2t
  case W.getRequestBodyType req of
    Just W.UrlEncoded -> do
      bs <- getBodyAction bodyInfo opts
      let wholeBody = BL.toChunks bs
      (formparams, fs) <- parseRequestBody wholeBody W.lbsBackEnd req -- NB this loads the whole body into memory
      return (convertBoth <$> formparams, convertKey <$> fs, [])
    Just (W.Multipart _) -> do
      (formparams, fs) <- sinkTempFiles istate prbo req
      return (convertBoth <$> formparams, [], convertKey <$> fs)
    Nothing -> do
      return ([], [], [])

sinkTempFiles :: InternalState -- global, to be initialized with the server
              -> ParseRequestBodyOptions -- " " with user input
              -> Request
              -> IO ([W.Param], [W.File FilePath])
sinkTempFiles istate o = parseRequestBodyEx o (W.tempFileBackEnd istate)

-- | Retrieve the entire body, using the cached chunks in the BodyInfo and reading any other
-- chunks if they still exist.
-- Mimic the previous behavior by throwing BodyPartiallyStreamed if the user has already
-- started reading the body by chunks.
getBodyAction :: BodyInfo -> RouteOptions -> IO (BL.ByteString)
getBodyAction (BodyInfo readProgress chunkBufferVar getChunk) opts =
  modifyMVar readProgress $ \index ->
    modifyMVar chunkBufferVar $ \bcb@(BodyChunkBuffer hasFinished chunks) -> do
      if | index > 0 -> E.throw BodyPartiallyStreamed
         | hasFinished -> return (bcb, (index, BL.fromChunks chunks))
         | otherwise -> do
             newChunks <- readRequestBody getChunk return (maxRequestBodySize opts)
             return $ (BodyChunkBuffer True (chunks ++ newChunks), (index, BL.fromChunks (chunks ++ newChunks)))

-- | Retrieve a chunk from the body at the index stored in the readProgress MVar.
-- Serve the chunk from the cached array if it's already present; otherwise read another
-- chunk from WAI and advance the index.
getBodyChunkAction :: BodyInfo -> IO BS.ByteString
getBodyChunkAction (BodyInfo readProgress chunkBufferVar getChunk) =
  modifyMVar readProgress $ \index ->
    modifyMVar chunkBufferVar $ \bcb@(BodyChunkBuffer hasFinished chunks) -> do
      if | index < length chunks -> return (bcb, (index + 1, chunks !! index))
         | hasFinished -> return (bcb, (index, mempty))
         | otherwise -> do
             newChunk <- getChunk
             return (BodyChunkBuffer (newChunk == mempty) (chunks ++ [newChunk]), (index + 1, newChunk))


-- Stolen from wai-extra's Network.Wai.Parse, modified to accept body as list of Bytestrings.
-- Reason: WAI's requestBody is an IO action that returns the body as chunks. Once read,
-- they can't be read again. We read them into a lazy Bytestring, so Scotty user can get
-- the raw body, even if they also want to call wai-extra's parsing routines.
parseRequestBody :: MonadIO m
                 => [B.ByteString]
                 -> W.BackEnd y
                 -> Request
                 -> m ([W.Param], [W.File y])
parseRequestBody bl s r =
    case W.getRequestBodyType r of
        Nothing -> return ([], [])
        Just rbt -> do
            mvar <- liftIO $ newMVar bl -- MVar is a bit of a hack so we don't have to inline
                                        -- large portions of Network.Wai.Parse
            let provider = modifyMVar mvar $ \bsold -> case bsold of
                                                []     -> return ([], B.empty)
                                                (b:bs) -> return (bs, b)
            liftIO $ W.sinkRequestBody s rbt provider
