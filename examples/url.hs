{-# LANGUAGE OverloadedStrings #-}
import Web.Spock

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import qualified Data.Map as M
import Data.Monoid (mconcat)
import qualified Data.Text as T

import Network.Wai.Middleware.RequestLogger

-- TODO:
-- Use some HTML combinators
-- Implement some kind of session and/or cookies
-- Add links
-- Figure out no parse?

main = spock 3000 $ do
    middleware logStdoutDev

    m <- liftIO $ newMVar (0::Int,M.empty)

    get "/" $ do
        html $ "<form method=get action=/shorten><input type=text name=url><input type=submit></form>"

    -- this should be a 'post', but WAI is broken right now
    get "/shorten" $ do
        url <- param "url"
        liftIO $ modifyMVar_ m $ \(i,db) -> return (i+1, M.insert i url db)
        redirect "/list"

    get "/list" $ do
        db <- liftIO $ withMVar m $ \(_,db) -> return (M.toList db)
        json db

    get "/:hash" $ do
        hash <- param "hash"
        (_,db) <- liftIO $ readMVar m
        case M.lookup (read $ T.unpack $ hash) db of
            Nothing -> raise $ mconcat ["URL hash #", hash, " not found in database!"]
            Just url -> redirect url
