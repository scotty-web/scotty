{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Network.Wai
import Network.Wai.Middleware.Gzip (gzip,def)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8)

main :: IO ()
main = scotty 6666 $ do
  middleware $ gzip def
  middleware logStdoutDev
  get "/" $ text "It works"
