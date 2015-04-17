{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Web.Scotty

import Network.Wai.Middleware.RequestLogger -- install wai-extra if you don't have this

import Data.Default.Class (def)
import Network.Wai.Handler.Warp (setPort)

-- Set some Scotty settings
opts :: Options
opts = def { verbose = 0
           , settings = setPort 4000 $ settings def
           }

-- This won't display anything at startup, and will listen on localhost:4000
main :: IO ()
main = scottyOpts opts $ do
    middleware logStdoutDev

    get "/" $ text "hello world"
