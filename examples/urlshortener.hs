{-# LANGUAGE OverloadedStrings #-}
{-# language DeriveAnyClass #-}
{-# language LambdaCase #-}
-- {-# language ScopedTypeVariables #-}
module Main (main) where

import Web.Scotty

import Control.Concurrent.MVar
import Control.Exception (Exception(..))
import Control.Monad.IO.Class
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import Data.Typeable (Typeable)

import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static

import Prelude ()
import Prelude.Compat

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
-- Note:
--   Scotty does not require blaze-html or
--   wai-middleware-static, but this example does
--       cabal install blaze-html wai-middleware-static
import Text.Blaze.Html.Renderer.Text (renderHtml)

-- TODO:
-- Implement some kind of session (#317) and/or cookies
-- Add links

data SessionError = UrlHashNotFound Int deriving (Typeable, Exception)
instance Show SessionError where
  show = \case
    UrlHashNotFound hash -> unwords ["URL hash #", show hash, " not found in database!"]

main :: IO ()
main = do
  m <- newMVar (0::Int, M.empty :: M.Map Int T.Text)
  scotty 3000 $ do
    middleware logStdoutDev
    middleware static

    get "/" $ do
        html $ renderHtml
             $ H.html $ do
                H.body $ do
                    H.form H.! method "post" H.! action "/shorten" $ do
                        H.input H.! type_ "text" H.! name "url"
                        H.input H.! type_ "submit"

    post "/shorten" $ do
        url <- captureParam "url"
        liftIO $ modifyMVar_ m $ \(i,db) -> return (i+1, M.insert i (T.pack url) db)
        redirect "/list"

    -- We have to be careful here, because this route can match pretty much anything.
    -- Thankfully, the type system knows that 'hash' must be an Int, so this route
    -- only matches if 'parseParam' can successfully parse the hash capture as an Int.
    -- Otherwise, the pattern match will fail and Scotty will continue matching
    -- subsequent routes.
    get "/:hash" $ do
        hash <- captureParam "hash"
        (_,db) <- liftIO $ readMVar m
        case M.lookup hash db of
            Nothing -> throw $ UrlHashNotFound hash
            Just url -> redirect url

    -- We put /list down here to show that it will not match the '/:hash' route above.
    get "/list" $ do
        (_,db) <- liftIO $ readMVar m
        json $ M.toList db
