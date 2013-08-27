{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
-- | An example of embedding a custom state monad into
-- Scotty's transformer stack, using an MVar to synchronize
-- the state globally.
--
-- Note: this example is somewhat simple, as our top level
-- is IO itself. The types of 'scottyT' and 'scottyAppT' are
-- general enough to allow a Scotty application to be
-- embedded into any MonadIO monad.
module Main where

import Control.Concurrent.MVar  
import Control.Monad.State hiding (get)

import Data.Default
import Data.Text.Lazy (pack)

import Web.Scotty.Trans

newtype AppState = AppState { tickCount :: Int }

instance Default AppState where
    def = AppState 0

newtype WebM a = WebM { runWebM :: StateT AppState IO a }
    deriving (Monad, MonadIO, MonadState AppState)

webM :: MonadTrans t => WebM a -> t WebM a
webM = lift            

main = do
    db <- newEmptyMVar
        -- Note that 'runM' is only called once, at startup.
    let runM m = do (r,s) <- runStateT (runWebM m) def
                    putMVar db s
                    return r
        -- 'runActionToIO' is called once per action.
        runActionToIO m = do s <- takeMVar db
                             (r,s') <- runStateT (runWebM m) s
                             putMVar db s'
                             return r 
            
    scottyT 3000 runM runActionToIO $ do 
        get "/" $ do
            c <- webM $ gets tickCount
            text $ pack $ show c

        get "/plusone" $ do
            webM $ modify $ \ st -> st { tickCount = tickCount st + 1 }
            redirect "/"

        get "/plustwo" $ do
            webM $ modify $ \ st -> st { tickCount = tickCount st + 2 }
            redirect "/"
