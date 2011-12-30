{-# LANGUAGE OverloadedStrings #-}
import Web.Spock

import Network.Wai.Middleware.RequestLogger

import Control.Monad.Trans
import Data.Monoid
import System.Random

main = spock 3000 $ do
    middleware logStdoutDev

    get "/" $ text "foobar"

    get "/foo" $ do
        Just v <- param "fooparam"
        html $ mconcat ["<h1>", v, "</h1>"]

    get "/foo/:bar/required" $ do
        Just v <- param "bar"
        html $ mconcat ["<h1>", v, "</h1>"]

    get "/404" $ file "404.html"

    get "/random" $ do
        g <- liftIO newStdGen
        json $ take 20 $ randomRs (1::Int,100) g

