{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

import Network.Wai.Middleware.RequestLogger

main :: IO ()
main = scotty 3000 $ do
  -- Add logging just to see what's going on.
  middleware logStdoutDev
  
  -- This parses and echoes XML data sent by the client.
  post "/xmlecho" $ do
    d <- xmlData
    xml d
