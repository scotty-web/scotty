{-# language DeriveDataTypeable #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
-- | This module is a "safe" variant of Network.Wai.Parse from wai-extras, to work around the usage of 'error' in the original.
--
-- It is meant to disappear once my patch to wai-extra https://github.com/yesodweb/wai/pull/964 is merged and the safe version of 'parseRequestBodyEx' is made available upstream.
module Web.Scotty.Internal.WaiParseSafe where

import Network.Wai.Parse (getRequestBodyType, fileContent, File, FileInfo(..), Param, BackEnd, RequestBodyType(..))

import qualified Control.Exception as E
import Control.Monad (guard, unless, when)
import Control.Monad.Trans.Resource (InternalState, allocate, register, release, runInternalState)
import Data.Bifunctor (bimap)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.CaseInsensitive (mk)
import Data.Int (Int64)
import Data.IORef
import Data.Maybe (catMaybes, fromMaybe)
import Data.Typeable
import Data.Word (Word8)
import qualified Network.HTTP.Types as H
import Network.Wai
import Network.Wai.Handler.Warp (InvalidRequest(..))


-- | A data structure that describes the behavior of
-- the parseRequestBodyEx function.
--
-- @since 3.0.16.0
data ParseRequestBodyOptions = ParseRequestBodyOptions
    { -- | The maximum length of a filename
      prboKeyLength             :: Maybe Int
    , -- | The maximum number of files.
      prboMaxNumFiles           :: Maybe Int
    , -- | The maximum filesize per file.
      prboMaxFileSize           :: Maybe Int64
    , -- | The maximum total filesize.
      prboMaxFilesSize          :: Maybe Int64
    , -- | The maximum size of the sum of all parameters
      prboMaxParmsSize          :: Maybe Int
    , -- | The maximum header lines per mime/multipart entry
      prboMaxHeaderLines        :: Maybe Int
    , -- | The maximum header line length per mime/multipart entry
      prboMaxHeaderLineLength   :: Maybe Int }

defaultParseRequestBodyOptions :: ParseRequestBodyOptions
defaultParseRequestBodyOptions = ParseRequestBodyOptions
    { prboKeyLength=Just 32
    , prboMaxNumFiles=Just 10
    , prboMaxFileSize=Nothing
    , prboMaxFilesSize=Nothing
    , prboMaxParmsSize=Just 65336
    , prboMaxHeaderLines=Just 32
    , prboMaxHeaderLineLength=Just 8190 }

-- | Parse the body of an HTTP request, limit resource usage.
-- The HTTP body can contain both parameters and files.
-- This function will return a list of key,value pairs
-- for all parameters, and a list of key,a pairs
-- for filenames. The a depends on the used backend that
-- is responsible for storing the received files.
--
-- since wai-extra-3.1.15 : throws 'RequestParseException' if something goes wrong
parseRequestBodyEx :: ParseRequestBodyOptions
                   -> BackEnd y
                   -> Request
                   -> IO ([Param], [File y])
parseRequestBodyEx o s r =
    case getRequestBodyType r of
        Nothing -> return ([], [])
        Just rbt -> sinkRequestBodyEx o s rbt (getRequestBodyChunk r)

-- | Throws 'RequestParseException' if something goes wrong
--
-- since wai-extra-3.1.15 : throws 'RequestParseException' if something goes wrong
sinkRequestBodyEx :: ParseRequestBodyOptions
                  -> BackEnd y
                  -> RequestBodyType
                  -> IO S.ByteString
                  -> IO ([Param], [File y])
sinkRequestBodyEx o s r body = do
    ref <- newIORef ([], [])
    let add x = atomicModifyIORef ref $ \(y, z) ->
            case x of
                Left y'  -> ((y':y, z), ())
                Right z' -> ((y, z':z), ())
    conduitRequestBodyEx o s r body add
    bimap reverse reverse <$> readIORef ref

conduitRequestBodyEx :: ParseRequestBodyOptions
                     -> BackEnd y
                     -> RequestBodyType
                     -> IO S.ByteString
                     -> (Either Param (File y) -> IO ())
                     -> IO ()
conduitRequestBodyEx o _ UrlEncoded rbody add = do
    -- NOTE: in general, url-encoded data will be in a single chunk.
    -- Therefore, I'm optimizing for the usual case by sticking with
    -- strict byte strings here.
    let loop size front = do
            bs <- rbody
            if S.null bs
                then return $ S.concat $ front []
                else do
                    let newsize = size + S.length bs
                    case prboMaxParmsSize o of
                        Just maxSize -> when (newsize > maxSize) $
                          E.throwIO $ MaxParamSizeExceeded newsize
                        Nothing -> return ()
                    loop newsize $ front . (bs:)
    bs <- loop 0 id
    mapM_ (add . Left) $ H.parseSimpleQuery bs
conduitRequestBodyEx o backend (Multipart bound) rbody add =
    parsePiecesEx o backend (S8.pack "--" `S.append` bound) rbody add



-- | Take one header or subheader line.
-- Since:  3.0.26
--  Throw 431 if headers too large.
takeLine :: Maybe Int -> Source -> IO (Maybe S.ByteString)
takeLine maxlen src =
    go ""
  where
    go front = do
        bs <- readSource src
        case maxlen of
            Just maxlen' -> when (S.length front > maxlen') $
              E.throwIO RequestHeaderFieldsTooLarge
            Nothing -> return ()
        if S.null bs
            then close front
            else push front bs

    close front = leftover src front >> return Nothing
    push front bs = do
        let (x, y) = S.break (== 10) bs -- LF
         in if S.null y
                then go $ front `S.append` x
                else do
                    when (S.length y > 1) $ leftover src $ S.drop 1 y
                    let res = front `S.append` x
                    case maxlen of
                        Just maxlen' -> when (S.length res > maxlen') $
                          E.throwIO RequestHeaderFieldsTooLarge
                        Nothing -> return ()
                    return . Just $ killCR res

-- | @since 3.1.15 : throws 'RequestParseException' if something goes wrong
takeLines' :: Maybe Int -> Maybe Int -> Source -> IO [S.ByteString]
takeLines' lineLength maxLines source =
    reverse <$> takeLines'' [] lineLength maxLines source

-- | @since 3.1.15 : throws 'RequestParseException' if something goes wrong
takeLines''
    :: [S.ByteString]
    -> Maybe Int
    -> Maybe Int
    -> Source
    -> IO [S.ByteString]
takeLines'' lines lineLength maxLines src = do
    case maxLines of
        Just maxLines' ->
            when (length lines > maxLines') $
                E.throwIO $ TooManyHeaderLines (length lines)
        Nothing -> return ()
    res <- takeLine lineLength src
    case res of
        Nothing -> return lines
        Just l
            | S.null l -> return lines
            | otherwise -> takeLines'' (l:lines) lineLength maxLines src



-- | @since 3.1.15 : throws 'RequestParseException' if something goes wrong
parsePiecesEx :: ParseRequestBodyOptions
              -> BackEnd y
              -> S.ByteString
              -> IO S.ByteString
              -> (Either Param (File y) -> IO ())
              -> IO ()
parsePiecesEx o sink bound rbody add =
    mkSource rbody >>= loop 0 0 0 0
  where
    loop :: Int -> Int -> Int -> Int64 -> Source -> IO ()
    loop numParms numFiles parmSize filesSize src = do
        _boundLine <- takeLine (prboMaxHeaderLineLength o) src
        res' <- takeLines' (prboMaxHeaderLineLength o)
            (prboMaxHeaderLines o) src
        unless (null res') $ do
            let ls' = map parsePair res'
            let x = do
                    cd <- lookup contDisp ls'
                    let ct = lookup contType ls'
                    let attrs = parseAttrs cd
                    name <- lookup "name" attrs
                    return (ct, name, lookup "filename" attrs)
            case x of
                Just (mct, name, Just filename) -> do
                    case prboKeyLength o of
                        Just maxKeyLength ->
                            when (S.length name > maxKeyLength) $
                                E.throwIO $ FilenameTooLong name maxKeyLength
                        Nothing -> return ()
                    case prboMaxNumFiles o of
                        Just maxFiles -> when (numFiles >= maxFiles) $
                          E.throwIO $ MaxFileNumberExceeded numFiles
                        Nothing -> return ()
                    let ct = fromMaybe "application/octet-stream" mct
                        fi0 = FileInfo filename ct ()
                        fs = catMaybes [ prboMaxFileSize o
                                       , subtract filesSize <$> prboMaxFilesSize o ]
                        mfs = if null fs then Nothing else Just $ minimum fs
                    ((wasFound, fileSize), y) <- sinkTillBound' bound name fi0 sink src mfs
                    let newFilesSize = filesSize + fileSize
                    add $ Right (name, fi0 { fileContent = y })
                    when wasFound $ loop numParms (numFiles + 1) parmSize newFilesSize src
                Just (_ct, name, Nothing) -> do
                    case prboKeyLength o of
                        Just maxKeyLength ->
                            when (S.length name > maxKeyLength) $
                                E.throwIO $ ParamNameTooLong name maxKeyLength
                        Nothing -> return ()
                    let seed = id
                    let iter front bs = return $ front . (:) bs
                    ((wasFound, _fileSize), front) <- sinkTillBound bound iter seed src
                        (fromIntegral <$> prboMaxParmsSize o)
                    let bs = S.concat $ front []
                    let x' = (name, bs)
                    let newParmSize = parmSize + S.length name + S.length bs
                    case prboMaxParmsSize o of
                        Just maxParmSize -> when (newParmSize > maxParmSize) $
                          E.throwIO $ MaxParamSizeExceeded newParmSize
                        Nothing -> return ()
                    add $ Left x'
                    when wasFound $ loop (numParms + 1) numFiles
                        newParmSize filesSize src
                _ -> do
                    -- ignore this part
                    let seed = ()
                        iter () _ = return ()
                    ((wasFound, _fileSize), ()) <- sinkTillBound bound iter seed src Nothing
                    when wasFound $ loop numParms numFiles parmSize filesSize src
      where
        contDisp = mk $ S8.pack "Content-Disposition"
        contType = mk $ S8.pack "Content-Type"
        parsePair s =
            let (x, y) = breakDiscard 58 s -- colon
             in (mk x, S.dropWhile (== 32) y) -- space

-- | Things that could go wrong while parsing a 'Request'
--
-- @since 3.1.15
data RequestParseException = MaxParamSizeExceeded Int
                           | ParamNameTooLong S.ByteString Int
                           | MaxFileNumberExceeded Int
                           | FilenameTooLong S.ByteString Int
                           | TooManyHeaderLines Int
                           deriving (Eq, Typeable)
instance E.Exception RequestParseException
instance Show RequestParseException where
  show = \case
    MaxParamSizeExceeded lmax -> unwords ["maximum parameter size exceeded:", show lmax]
    ParamNameTooLong s lmax -> unwords ["parameter name", S8.unpack s, "is too long:", show lmax]
    MaxFileNumberExceeded lmax -> unwords ["maximum number of files exceeded:", show lmax]
    FilenameTooLong fn lmax ->
      unwords ["file name", S8.unpack fn, "too long:", show lmax]
    TooManyHeaderLines nmax -> unwords ["Too many lines in mime/multipart header:", show nmax]


-- | INTERNAL

data Source = Source (IO S.ByteString) (IORef S.ByteString)

mkSource :: IO S.ByteString -> IO Source
mkSource f = do
    ref <- newIORef S.empty
    return $ Source f ref

readSource :: Source -> IO S.ByteString
readSource (Source f ref) = do
    bs <- atomicModifyIORef ref $ \bs -> (S.empty, bs)
    if S.null bs
        then f
        else return bs
{- HLint ignore readSource "Use tuple-section" -}

leftover :: Source -> S.ByteString -> IO ()
leftover (Source _ ref) = writeIORef ref



data Bound = FoundBound S.ByteString S.ByteString
           | NoBound
           | PartialBound
    deriving (Eq, Show)

findBound :: S.ByteString -> S.ByteString -> Bound
findBound b bs = handleBreak $ S.breakSubstring b bs
  where
    handleBreak (h, t)
        | S.null t = go [lowBound..S.length bs - 1]
        | otherwise = FoundBound h $ S.drop (S.length b) t

    lowBound = max 0 $ S.length bs - S.length b

    go [] = NoBound
    go (i:is)
        | mismatch [0..S.length b - 1] [i..S.length bs - 1] = go is
        | otherwise =
            let endI = i + S.length b
             in if endI > S.length bs
                    then PartialBound
                    else FoundBound (S.take i bs) (S.drop endI bs)
    mismatch [] _ = False
    mismatch _ [] = False
    mismatch (x:xs) (y:ys)
        | S.index b x == S.index bs y = mismatch xs ys
        | otherwise = True

sinkTillBound' :: S.ByteString
               -> S.ByteString
               -> FileInfo ()
               -> BackEnd y
               -> Source
               -> Maybe Int64
               -> IO ((Bool, Int64), y)
sinkTillBound' bound name fi sink src max' = do
    (next, final) <- wrapTillBound bound src max'
    y <- sink name fi next
    b <- final
    return (b, y)

data WTB = WTBWorking (S.ByteString -> S.ByteString)
         | WTBDone Bool
wrapTillBound :: S.ByteString -- ^ bound
              -> Source
              -> Maybe Int64
              -> IO (IO S.ByteString, IO (Bool, Int64)) -- ^ Bool indicates if the bound was found
wrapTillBound bound src max' = do
    ref <- newIORef $ WTBWorking id
    sref <- newIORef (0 :: Int64)
    return (go ref sref, final ref sref)
  where
    final ref sref = do
        x <- readIORef ref
        case x of
            WTBWorking _ -> error "wrapTillBound did not finish"
            WTBDone y -> do
                siz <- readIORef sref
                return (y, siz)

    go ref sref = do
        state <- readIORef ref
        case state of
            WTBDone _ -> return S.empty
            WTBWorking front -> do
                bs <- readSource src
                cur <- atomicModifyIORef' sref $ \ cur ->
                    let new = cur + fromIntegral (S.length bs) in (new, new)
                case max' of
                   Just max'' | cur > max'' -> E.throwIO PayloadTooLarge
                   _ -> return ()
                if S.null bs
                    then do
                        writeIORef ref $ WTBDone False
                        return $ front bs
                    else push $ front bs
      where
        push bs = do
            case findBound bound bs of
                FoundBound before after -> do
                    let before' = killCRLF before
                    leftover src after
                    writeIORef ref $ WTBDone True
                    return before'
                NoBound -> do
                    -- don't emit newlines, in case it's part of a bound
                    let (toEmit, front') =
                            if not (S8.null bs) && S8.last bs `elem` ['\r','\n']
                                then let (x, y) = S.splitAt (S.length bs - 2) bs
                                      in (x, S.append y)
                                else (bs, id)
                    writeIORef ref $ WTBWorking front'
                    if S.null toEmit
                        then go ref sref
                        else return toEmit
                PartialBound -> do
                    writeIORef ref $ WTBWorking $ S.append bs
                    go ref sref

sinkTillBound :: S.ByteString
              -> (x -> S.ByteString -> IO x)
              -> x
              -> Source
              -> Maybe Int64
              -> IO ((Bool, Int64), x)
sinkTillBound bound iter seed0 src max' = do
    (next, final) <- wrapTillBound bound src max'
    let loop seed = do
            bs <- next
            if S.null bs
                then return seed
                else iter seed bs >>= loop
    seed <- loop seed0
    b <- final
    return (b, seed)


parseAttrs :: S.ByteString -> [(S.ByteString, S.ByteString)]
parseAttrs = map go . S.split 59 -- semicolon
  where
    tw = S.dropWhile (== 32) -- space
    dq s = if S.length s > 2 && S.head s == 34 && S.last s == 34 -- quote
                then S.tail $ S.init s
                else s
    go s =
        let (x, y) = breakDiscard 61 s -- equals sign
         in (tw x, dq $ tw y)

-- string utilities

killCRLF :: S.ByteString -> S.ByteString
killCRLF bs
    | S.null bs || S.last bs /= 10 = bs -- line feed
    | otherwise = killCR $ S.init bs

killCR :: S.ByteString -> S.ByteString
killCR bs
    | S.null bs || S.last bs /= 13 = bs -- carriage return
    | otherwise = S.init bs

breakDiscard :: Word8 -> S.ByteString -> (S.ByteString, S.ByteString)
breakDiscard w s =
    let (x, y) = S.break (== w) s
     in (x, S.drop 1 y)
