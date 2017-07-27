{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, RecordWildCards,
             OverloadedStrings, RankNTypes, ScopedTypeVariables, MultiWayIf #-}
module Web.Scotty.Body where

import           Control.Concurrent.MVar
import           Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Maybe
import           GHC.Exception
import           Network.Wai (Request(..))
import qualified Network.Wai.Parse as Parse hiding (parseRequestBody)
import           Web.Scotty.Action
import           Web.Scotty.Internal.Types
import           Web.Scotty.Util

getBodyInfo :: forall m. (MonadIO m) => Request -> m BodyInfo
getBodyInfo req = liftIO $ do
  readProgress <- newMVar (BodyReadProgress 0 False)
  chunkBuffer <- newMVar []
  return $ BodyInfo readProgress chunkBuffer (requestBody req)

cloneBodyInfo :: forall m. (MonadIO m) => BodyInfo -> m BodyInfo
cloneBodyInfo (BodyInfo readProgress chunkBuffer getChunk) = liftIO $ do
  BodyReadProgress _index hasFinished <- readMVar readProgress
  cleanProgress <- newMVar $ BodyReadProgress 0 hasFinished
  return $ BodyInfo cleanProgress chunkBuffer getChunk

getFormParamsAndFilesAction :: Request -> BodyInfo -> IO ([Param], [File])
getFormParamsAndFilesAction req bodyInfo = do
  let shouldParseBody = isJust $ Parse.getRequestBodyType req

  if | shouldParseBody -> do
         bs <- getBodyAction bodyInfo
         let wholeBody = BL.toChunks bs
         (formparams, fs) <- parseRequestBody wholeBody Parse.lbsBackEnd req
         let convert (k, v) = (strictByteStringToLazyText k, strictByteStringToLazyText v)
         return (fmap convert formparams, [(strictByteStringToLazyText x, y) | (x, y) <- fs])
     | otherwise -> return ([], [])


getBodyAction :: BodyInfo -> IO (BL.ByteString)
getBodyAction (BodyInfo readProgress chunkBufferVar getChunk) =
  modifyMVar readProgress $ \brp@(BodyReadProgress index hasFinished) ->
    if | index > 0 -> throw BodyPartiallyStreamed
       | hasFinished -> do
           chunks <- readMVar chunkBufferVar
           return (brp, BL.fromChunks chunks)
       | otherwise -> do
           newChunks <- takeAll getChunk return
           oldChunks <- modifyMVar chunkBufferVar $ \oldChunks -> return (oldChunks ++ newChunks, oldChunks)
           return $ (brp { hasFinishedReadingChunks = True}, BL.fromChunks (oldChunks ++ newChunks))


getBodyChunkAction :: BodyInfo -> IO BS.ByteString
getBodyChunkAction (BodyInfo readProgress chunkBufferVar getChunk) =
  modifyMVar readProgress $ \brp@(BodyReadProgress index hasFinished) ->
    modifyMVar chunkBufferVar $ \chunkBuffer -> do
      if | index < length chunkBuffer -> do
             let chunk = chunkBuffer !! index
             return (chunkBuffer, (brp { bodyReaderIndex = index + 1 }, chunk))
         | hasFinished -> return (chunkBuffer, (brp, mempty))
         | otherwise -> do
             newChunk <- getChunk
             return (chunkBuffer ++ [newChunk], (brp { bodyReaderIndex = index + 1 }, newChunk))



-- If they call body and they've already gotten access to the bodyReader, throw BodyPartiallyStreamed
-- Otherwise, do a series of calls to get all the chunks then a) cache all the chunks, and b) return the concatenated version
-- At this point, the user may get the bodyReader, which will read from the MVar containing the cached chunks

-- To summarize:
-- Calling bodyReader followed by body should throw BodyPartiallyStreamed (actually we can improve on this and just return the entire body. unless we care about using the additional memory?)
-- Calling body followed by bodyReader should work normally


takeAll :: (IO B.ByteString) -> ([B.ByteString] -> IO [B.ByteString]) -> IO [B.ByteString]
takeAll getChunk prefix = getChunk >>= \b -> if B.null b then prefix [] else takeAll getChunk (prefix . (b:))

    --     safeBodyReader :: IO B.ByteString
    --     safeBodyReader =  do
    --       state <- takeMVar bodyState
    --       let direct = putMVar bodyState BodyCorrupted >> (requestBody req)
    --       case state of
    --         s@(BodyCached _ []) -> (putMVar bodyState s) >> return B.empty
    --         BodyCached b (chunk:rest) -> (putMVar bodyState $ BodyCached b rest) >> return chunk
    --         BodyUntouched -> direct
    --         BodyCorrupted -> direct

    --     bs :: IO BL.ByteString
    --     bs = do
    --       state <- takeMVar bodyState
    --       case state of
    --         s@(BodyCached b _) -> (putMVar bodyState s) >> return b
    --         BodyCorrupted -> throw BodyPartiallyStreamed
    --         BodyUntouched ->
    --           do chunks <- takeAll return
    --              let b = BL.fromChunks chunks
    --              putMVar bodyState $ BodyCached b chunks
    --              return b

    --     shouldParseBody = isJust $ Parse.getRequestBodyType req

    -- (formparams, fs) <- if shouldParseBody
    --   then liftIO $ do wholeBody <- BL.toChunks `fmap` bs
    --                    parseRequestBody wholeBody Parse.lbsBackEnd req
    --   else return ([], [])

    -- return $ if shouldParseBody then Just (BodyInfo ) else Nothing

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
