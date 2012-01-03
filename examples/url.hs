{-# LANGUAGE OverloadedStrings #-}
import Web.Spock

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import qualified Data.Map as M
import Data.Monoid (mconcat)
import qualified Data.Text.Lazy as T

import Network.HTTP.Types
import Network.Wai.Middleware.RequestLogger

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import Text.Blaze.Renderer.Text (renderHtml)

-- TODO:
-- Implement some kind of session and/or cookies
-- Add links

main :: IO ()
main = spock 3000 $ do
    middleware logStdoutDev

    m <- liftIO $ newMVar (0::Int,M.empty)

    get "/" $ do
        html $ renderHtml
             $ H.html $ do
                H.body $ do
                    H.form H.! method "get" H.! action "/shorten" $ do
                        H.input H.! type_ "text" H.! name "url"
                        H.input H.! type_ "submit"

    -- this should be a 'post', but WAI is broken right now
    get "/shorten" $ do
        url <- param "url"
        liftIO $ modifyMVar_ m $ \(i,db) -> return (i+1, M.insert i url db)
        redirect "/list"

    get "/list" $ do
        db <- liftIO $ withMVar m $ \(_,db) -> return (M.toList db)
        json db

    -- todo: static serving middleware
    get "/favicon.ico" $ status status404

    get "/:hash" $ do
        hash <- param "hash"
        (_,db) <- liftIO $ readMVar m
        case M.lookup (read $ T.unpack $ hash) db of
            Nothing -> raise $ mconcat ["URL hash #", hash, " not found in database!"]
            Just url -> redirect url
