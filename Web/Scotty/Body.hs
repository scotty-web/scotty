{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
module Web.Scotty.Body (
  newBodyInfo,
  cloneBodyInfo

  , getFormParamsAndFilesAction
  , getBodyAction
  , getBodyChunkAction
  -- wai-extra
  , W.RequestParseException(..)
  ) where

import           Control.Concurrent.MVar
import           Control.Monad.IO.Class
import Control.Monad.Trans.Resource (InternalState)
import Data.Bifunctor (first, bimap)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified GHC.Exception as E (throw)
import           Network.Wai (Request(..), getRequestBodyChunk)
import qualified Network.Wai.Handler.Warp as Warp (InvalidRequest(..))
import qualified Network.Wai.Parse as W (File, Param, getRequestBodyType, lbsBackEnd, tempFileBackEnd, RequestBodyType(..), sinkRequestBody, sinkRequestBodyEx, RequestParseException(..), ParseRequestBodyOptions, defaultParseRequestBodyOptions, BackEnd)
-- import UnliftIO (MonadUnliftIO(..))
import UnliftIO.Exception (Handler(..), catches, throwIO)

import           Web.Scotty.Internal.Types (BodyInfo(..), BodyChunkBuffer(..), BodyPartiallyStreamed(..), RouteOptions(..), File, ScottyException(..), Param)
import           Web.Scotty.Util (readRequestBody, decodeUtf8Lenient)


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
--
-- NB : catches exceptions from 'warp' and 'wai-extra' and wraps them into 'ScottyException'
getFormParamsAndFilesAction ::
  InternalState
  -> W.ParseRequestBodyOptions
  -> Request -- ^ only used for its body type
  -> BodyInfo -- ^ the request body contents are read from here
  -> RouteOptions
  -> IO ([Param], [File FilePath])
getFormParamsAndFilesAction istate prbo req bodyInfo opts = do
  let
    bs2t = decodeUtf8Lenient
    convertBoth = bimap bs2t bs2t
    convertKey = first bs2t
  bs <- getBodyAction bodyInfo opts
  let
    wholeBody = BL.toChunks bs
  (formparams, fs) <- parseRequestBodyExBS istate prbo wholeBody (W.getRequestBodyType req) `catches` handleWaiParseSafeExceptions
  return (convertBoth <$> formparams, convertKey <$> fs)

-- | Wrap exceptions from upstream libraries into 'ScottyException'
handleWaiParseSafeExceptions :: MonadIO m => [Handler m a]
handleWaiParseSafeExceptions = [h1, h2]
  where
    h1 = Handler (\ (e :: W.RequestParseException ) -> throwIO $ WaiRequestParseException e)
    h2 = Handler (\(e :: Warp.InvalidRequest) -> throwIO $ WarpRequestException e)

-- | Adapted from wai-extra's Network.Wai.Parse, modified to accept body as list of Bytestrings.
-- Reason: WAI's requestBody is an IO action that returns the body as chunks. Once read,
-- they can't be read again. We read them into a lazy Bytestring, so Scotty user can get
-- the raw body, even if they also want to call wai-extra's parsing routines.
parseRequestBodyExBS :: MonadIO m =>
                        InternalState
                     -> W.ParseRequestBodyOptions
                     -> [B.ByteString]
                     -> Maybe W.RequestBodyType
                     -> m ([W.Param], [W.File FilePath])
parseRequestBodyExBS istate o bl rty =
    case rty of
        Nothing -> return ([], [])
        Just rbt -> do
            mvar <- liftIO $ newMVar bl -- MVar is a bit of a hack so we don't have to inline
                                        -- large portions of Network.Wai.Parse
            let provider = modifyMVar mvar $ \bsold -> case bsold of
                                                []     -> return ([], B.empty)
                                                (b:bs) -> return (bs, b)
            liftIO $ W.sinkRequestBodyEx o (W.tempFileBackEnd istate) rbt provider


-- sinkReqBodyWith rty getChunk = \case
--   Nothing -> do
--     (ps, fs) <- W.sinkRequestBodyEx W.defaultParseRequestBodyOptions W.lbsBackEnd rty getChunk
--     pure (ps, Right fs)
--   Just (istate, rbo) -> do
--     (ps, fs) <- W.sinkRequestBodyEx rbo (W.tempFileBackEnd istate) rty getChunk
--     pure (ps, Left fs)


-- | Retrieve the entire body, using the cached chunks in the BodyInfo and reading any other
-- chunks if they still exist.
-- Mimic the previous behavior by throwing 'BodyPartiallyStreamed' if the user has already
-- started reading the body by chunks.
--
-- throw 'ScottyException' if request body too big
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
             return (BodyChunkBuffer (B.null newChunk) (chunks ++ [newChunk]), (index + 1, newChunk))

