{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{-
    An example of embedding a custom monad into Scotty's transformer
    stack, using ReaderT to provide access to a global state.
-}
module Main where

import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, asks, lift, runReaderT)
import Data.Default.Class (def)
import Data.Text.Lazy (Text, pack)
import Prelude ()
import Prelude.Compat
import Web.Scotty.Trans (ScottyT, get, scottyOptsT, text)

data Config = Config
  { environment :: String
  } deriving (Eq, Read, Show)

newtype ConfigM a = ConfigM
  { runConfigM :: ReaderT Config IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

application :: ScottyT Text ConfigM ()
application = do
  get "/" $ do
    e <- lift $ asks environment
    text $ pack $ show e

main :: IO ()
main = scottyOptsT def runIO application where
  runIO :: ConfigM a -> IO a
  runIO m = runReaderT (runConfigM m) config

  config :: Config
  config = Config
    { environment = "Development"
    }
