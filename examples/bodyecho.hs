{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Web.Scotty

import Control.Monad.IO.Class (liftIO)
import qualified Blaze.ByteString.Builder as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Lazy as T

main :: IO ()
main = scotty 3000 $ do
    post "/echo" $ do
        rd <- bodyReader
        stream $ ioCopy rd $ return ()

    post "/count" $ do
        wb <- body -- this must happen before first 'rd'
        rd <- bodyReader
        let step acc = do 
              chunk <- rd
              putStrLn "got a chunk"
              let len = BS.length chunk
              if len > 0 
                then step $ acc + len
                else return acc
        len <- liftIO $ step 0
        text $ T.pack $ "uploaded " ++ show len ++ " bytes, wb len is " ++ show (BSL.length wb)


ioCopy :: IO BS.ByteString -> IO () -> (B.Builder -> IO ()) -> IO () -> IO ()
ioCopy reader close write flush = step >> flush where
   step = do chunk <- reader
             if (BS.length chunk > 0) 
               then (write $ B.insertByteString chunk) >> step
               else close
