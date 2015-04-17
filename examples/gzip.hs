{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Gzip

import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
    -- Note that files are not gzip'd by the default settings.
    middleware $ gzip $ def { gzipFiles = GzipCompress }
    middleware logStdoutDev

    -- gzip a normal response
    get "/" $ text "It works"

    -- gzip a file response (note non-default gzip settings above)
    get "/afile" $ do
        setHeader "content-type" "text/plain"
        file "gzip.hs"
