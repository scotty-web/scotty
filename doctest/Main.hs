module Main where

import Test.DocTest (doctest)

main :: IO ()
main = doctest [
  "Web/Scotty.hs"
  , "Web/Scotty/Trans.hs"
  , "-XOverloadedStrings"
  , "-XLambdaCase"
  ]
