{-# LANGUAGE OverloadedStrings #-}
import Web.Spock

import Network.Wai.Middleware.RequestLogger

import Control.Monad.Trans
import Data.Monoid
import System.Random

import Network.HTTP.Types (status302)

main = spock 3000 $ do
    -- Add any WAI middleware, they are run top-down.
    middleware logStdoutDev

    -- To demonstrate that routes are matched top-down.
    get "/" $ text "foobar"
    get "/" $ text "barfoo"

    -- Using a parameter in the query string. If it has
    -- not been given, a 500 page is generated.
    get "/foo" $ do
        v <- param "fooparam"
        html $ mconcat ["<h1>", v, "</h1>"]

    -- An uncaught error becomes a 500 page.
    get "/raise" $ raise "some error here"

    -- You can set status and headers directly.
    get "/redirect-custom" $ do
        status status302
        header "Location" "http://www.google.com"
        -- note first arg to header is NOT case-sensitive

    -- redirects preempt execution
    get "/redirect" $ do
        redirect "http://www.google.com"
        raise "this error is never reached"

    -- Of course you can catch your own errors.
    get "/rescue" $ do
        (do raise "a rescued error"; redirect "http://www.we-never-go-here.com")
        `rescue` (\m -> text $ "we recovered from " `mappend` m)

    -- Parts of the URL that start with a colon match
    -- any string, and capture that value as a parameter.
    -- URL captures take precedence over query string parameters.
    get "/foo/:bar/required" $ do
        v <- param "bar"
        html $ mconcat ["<h1>", v, "</h1>"]

    -- Files are streamed directly to the client.
    get "/404" $ file "404.html"

    -- You can do IO with liftIO, and you can return JSON content.
    get "/random" $ do
        g <- liftIO newStdGen
        json $ take 20 $ randomRs (1::Int,100) g

{- If you don't want to use Warp as your webserver,
   you can use any WAI handler.

import Network.Wai.Handler.FastCGI (run)

main = do
    myApp <- spockApp $ do
        get "/" $ text "hello world"

    run myApp
-}

