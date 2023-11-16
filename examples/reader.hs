{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{-
    An example of embedding a custom monad into Scotty's transformer
    stack, using ReaderT to provide access to a global state.
-}
module Main where

import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, asks, lift, runReaderT)
import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import Data.Text.Lazy (pack)
import Web.Scotty.Trans (ScottyT, defaultOptions, get, scottyOptsT, text)

data Config = Config
  { environment :: String
  } deriving (Eq, Read, Show)

newtype ConfigM a = ConfigM
  { runConfigM :: ReaderT Config IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config, MonadUnliftIO)

application :: ScottyT ConfigM ()
application = do
  get "/" $ do
    e <- lift $ asks environment
    text $ pack $ show e

main :: IO ()
main = scottyOptsT defaultOptions runIO application where
  runIO :: ConfigM a -> IO a
  runIO m = runReaderT (runConfigM m) config

  config :: Config
  config = Config
    { environment = "Development"
    }
