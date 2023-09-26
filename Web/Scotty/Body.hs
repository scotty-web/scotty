{-# LANGUAGE FlexibleContexts, FlexibleInstances, RecordWildCards,
             OverloadedStrings, MultiWayIf #-}
module Web.Scotty.Body (
  newBodyInfo,
  cloneBodyInfo

  , getFormParamsAndFilesAction
  , getBodyAction
  , getBodyChunkAction
  ) where

import           Control.Concurrent.MVar
import           Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Maybe
import           GHC.Exception
import           Network.Wai (Request(..), getRequestBodyChunk)
import qualified Network.Wai.Parse as Parse hiding (parseRequestBody)
import           Web.Scotty.Action
import           Web.Scotty.Internal.Types (BodyInfo(..), BodyChunkBuffer(..), BodyPartiallyStreamed(..))
import           Web.Scotty.Util

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

-- | Get the form params and files from the request. Requires reading the whole body.
getFormParamsAndFilesAction :: Request -> BodyInfo -> IO ([Param], [Parse.File BL.ByteString])
getFormParamsAndFilesAction req bodyInfo = do
  let shouldParseBody = isJust $ Parse.getRequestBodyType req

  if shouldParseBody
    then
    do
      bs <- getBodyAction bodyInfo
      let wholeBody = BL.toChunks bs
      (formparams, fs) <- parseRequestBody wholeBody Parse.lbsBackEnd req
      let convert (k, v) = (strictByteStringToLazyText k, strictByteStringToLazyText v)
      return (convert <$> formparams, fs)
    else
    return ([], [])

-- | Retrieve the entire body, using the cached chunks in the BodyInfo and reading any other
-- chunks if they still exist.
-- Mimic the previous behavior by throwing BodyPartiallyStreamed if the user has already
-- started reading the body by chunks.
getBodyAction :: BodyInfo -> IO (BL.ByteString)
getBodyAction (BodyInfo readProgress chunkBufferVar getChunk) =
  modifyMVar readProgress $ \index ->
    modifyMVar chunkBufferVar $ \bcb@(BodyChunkBuffer hasFinished chunks) -> do
      if | index > 0 -> throw BodyPartiallyStreamed
         | hasFinished -> return (bcb, (index, BL.fromChunks chunks))
         | otherwise -> do
             newChunks <- takeAll getChunk return
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

-- | Call getChunk repeatedly until it returns an empty chunk, and return a list of all the
-- chunks read.
takeAll :: (IO B.ByteString) -> ([B.ByteString] -> IO [B.ByteString]) -> IO [B.ByteString]
takeAll getChunk prefix = getChunk >>= \b -> if B.null b then prefix [] else takeAll getChunk (prefix . (b:))


-- Stolen from wai-extra's Network.Wai.Parse, modified to accept body as list of Bytestrings.
-- Reason: WAI's requestBody is an IO action that returns the body as chunks. Once read,
-- they can't be read again. We read them into a lazy Bytestring, so Scotty user can get
-- the raw body, even if they also want to call wai-extra's parsing routines.
parseRequestBody :: MonadIO m
                 => [B.ByteString]
                 -> Parse.BackEnd y
                 -> Request
                 -> m ([Parse.Param], [Parse.File y])
parseRequestBody bl s r =
    case Parse.getRequestBodyType r of
        Nothing -> return ([], [])
        Just rbt -> do
            mvar <- liftIO $ newMVar bl -- MVar is a bit of a hack so we don't have to inline
                                        -- large portions of Network.Wai.Parse
            let provider = modifyMVar mvar $ \bsold -> case bsold of
                                                []     -> return ([], B.empty)
                                                (b:bs) -> return (bs, b)
            liftIO $ Parse.sinkRequestBody s rbt provider
