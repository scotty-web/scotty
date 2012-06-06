{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

import Network.Wai.Middleware.RequestLogger -- install wai-extra if you don't have this

import Data.Default (def)
import Network.Wai.Handler.Warp (settingsPort)

-- Set some Scotty settings
opts :: Options
opts = def { verbose = 0
           , settings = (settings def) { settingsPort = 4000 }
           }

-- This won't display anything at startup, and will listen on localhost:4000
main :: IO ()
main = scottyOpts opts $ do
    middleware logStdoutDev

    get "/" $ text "hello world"
