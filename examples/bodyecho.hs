{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty

import Control.Monad.IO.Class (liftIO)
import qualified Blaze.ByteString.Builder as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

main :: IO ()
main = scotty 3000 $ do
    post "/" $ do
        rd <- bodyReader
        stream $ ioCopy rd $ return ()

ioCopy :: IO BS.ByteString -> IO () -> (B.Builder -> IO ()) -> IO () -> IO ()
ioCopy reader close write flush = step >> flush where
   step = do chunk <- reader
             if (BS.length chunk > 0) 
               then (write $ B.insertByteString chunk) >> step
               else close
