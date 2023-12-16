{-# LANGUAGE CPP #-}
module Main where

#if __GLASGOW_HASKELL__ >= 946
import Test.DocTest (doctest)

-- 1. Our current doctests require a number of imports that scotty doesn't need
-- 2. declaring doctest helper functions in this module doesn't seem to work
-- 3. cabal tests cannot have exposed modules?
-- 4. GHCi only started supporting multiline imports since 9.4.6 ( https://gitlab.haskell.org/ghc/ghc/-/issues/20473 )
-- so lacking a better option we no-op doctest for older GHCs

main :: IO ()
main = doctest [
  "Web/Scotty.hs"
  , "Web/Scotty/Trans.hs"
  , "-XOverloadedStrings"
  , "-XLambdaCase"
  ]
#else
main :: IO ()
main = pure ()
#endif
