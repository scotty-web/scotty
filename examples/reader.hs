{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, asks, lift, runReaderT)
import Data.Default (def)
import Data.Text.Lazy (Text, pack)
import Web.Scotty.Trans (ScottyT, get, scottyOptsT, text)

data State = State
  { environment :: String
  } deriving (Eq, Read, Show)

newtype StateM a = StateM
  { runStateM :: ReaderT State IO a
  } deriving (Monad, MonadIO, MonadReader State)

application :: ScottyT Text StateM ()
application = do
  get "/" $ do
    e <- lift $ asks environment
    text $ pack $ show e

main :: IO ()
main = scottyOptsT def runM runIO application where
  runM :: StateM a -> IO a
  runM m = runReaderT (runStateM m) state

  runIO :: StateM a -> IO a
  runIO = runM

  state :: State
  state = State
    { environment = "Development"
    }
