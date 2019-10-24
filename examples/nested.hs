{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Network.Wai
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types.Status
import Data.Monoid (mconcat)

simpleApp :: Application
simpleApp _ respond = do
    putStrLn "I've done some IO here"
    respond $ responseLBS
        status200
        [("Content-Type", "text/plain")]
        "Hello, Web!"

scottApp :: IO Application
scottApp = scottyApp $ do

    get "/" $ do
        html $ mconcat ["<h1>Scotty, bean me up!</h1>"]

    get "/test/:word" $ do
        beam <- param "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

    get "/nested" $ nested simpleApp

    notFound $ do
      r <- request
      html (TL.pack (show (pathInfo r)))

      -- For example, returns path info: ["other","qwer","adxf","jkashdfljhaslkfh","qwer"]
      -- for request http://localhost:3000/other/qwer/adxf/jkashdfljhaslkfh/qwer

main :: IO ()
main = do

  otherApp <- scottApp

  scotty 3000 $ do

    get "/" $ do
        html $ mconcat ["<h1>Scotty, bean me up!</h1>"]

    get "/test/:word" $ do
        beam <- param "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

    get "/simple" $ nested simpleApp

    get "/other" $ nested otherApp

    get (regex "/other/.*") $ nested otherApp


