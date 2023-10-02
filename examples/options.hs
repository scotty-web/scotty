{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Web.Scotty

import Network.Wai.Middleware.RequestLogger -- install wai-extra if you don't have this

import Network.Wai.Handler.Warp (setPort)

-- Set some Scotty settings
opts :: Options
opts = defaultOptions { verbose = 0
                      , settings = setPort 4000 $ settings defaultOptions
                      }

-- This won't display anything at startup, and will listen on localhost:4000
main :: IO ()
main = scottyOpts opts $ do
    middleware logStdoutDev

    get "/" $ text "hello world"
