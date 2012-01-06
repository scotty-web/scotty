{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Data.Char (isDigit)
import qualified Data.Map as M
import Data.Monoid (mconcat)
import qualified Data.Text.Lazy as T

import Network.HTTP.Types
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import Text.Blaze.Renderer.Text (renderHtml)

-- TODO:
-- Implement some kind of session and/or cookies
-- Add links

main :: IO ()
main = scotty 3000 $ do
    middleware logStdoutDev
    middleware static

    m <- liftIO $ newMVar (0::Int,M.empty)

    get "/" $ do
        html $ renderHtml
             $ H.html $ do
                H.body $ do
                    H.form H.! method "post" H.! action "/shorten" $ do
                        H.input H.! type_ "text" H.! name "url"
                        H.input H.! type_ "submit"

    post "/shorten" $ do
        url <- param "url"
        liftIO $ modifyMVar_ m $ \(i,db) -> return (i+1, M.insert i url db)
        redirect "/list"

    -- we have to be careful here, because this route can match pretty much anything
    -- if the hash is not composed entirely of digits, we use continue to abort this
    -- route and keep matching.
    get "/:hash" $ do
        hash <- param "hash"
        if T.all isDigit hash
        then do (_,db) <- liftIO $ readMVar m
                case M.lookup (read $ T.unpack $ hash) db of
                    Nothing -> raise $ mconcat ["URL hash #", hash, " not found in database!"]
                    Just url -> redirect url
        else continue

    -- we put /list down here to show that continue works
    get "/list" $ do
        db <- liftIO $ withMVar m $ \(_,db) -> return (M.toList db)
        json db

