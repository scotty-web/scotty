{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Web.Scotty

import Network.Wai.Middleware.RequestLogger -- install wai-extra if you don't have this

import Control.Monad
import Control.Monad.Trans
import System.Random (newStdGen, randomRs)

import Network.HTTP.Types (status302)

import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.String (fromString)
import Prelude ()
import Prelude.Compat

main :: IO ()
main = scotty 3000 $ do
    -- Add any WAI middleware, they are run top-down.
    middleware logStdoutDev

--    get (function $ \req -> Just [("version", T.pack $ show $ httpVersion req)]) $ do
--        v <- param "version"
--        text v

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
        setHeader "Location" "http://www.google.com"
        -- note first arg to header is NOT case-sensitive

    -- redirects preempt execution
    get "/redirect" $ do
        void $ redirect "http://www.google.com"
        raise "this error is never reached"

    -- Of course you can catch your own errors.
    get "/rescue" $ do
        (do void $ raise "a rescued error"; redirect "http://www.we-never-go-here.com")
        `rescue` (\m -> text $ "we recovered from " `mappend` m)

    -- Parts of the URL that start with a colon match
    -- any string, and capture that value as a parameter.
    -- URL captures take precedence over query string parameters.
    get "/foo/:bar/required" $ do
        v <- param "bar"
        html $ mconcat ["<h1>", v, "</h1>"]

    -- Files are streamed directly to the client.
    get "/404" $ file "404.html"

    -- You can stop execution of this action and keep pattern matching routes.
    get "/random" $ do
        void next
        redirect "http://www.we-never-go-here.com"

    -- You can do IO with liftIO, and you can return JSON content.
    get "/random" $ do
        g <- liftIO newStdGen
        json $ take 20 $ randomRs (1::Int,100) g

    get "/ints/:is" $ do
        is <- param "is"
        json $ [(1::Int)..10] ++ is

    get "/setbody" $ do
        html $ mconcat ["<form method=POST action=\"readbody\">"
                       ,"<input type=text name=something>"
                       ,"<input type=submit>"
                       ,"</form>"
                       ]

    post "/readbody" $ do
        b <- body
        text $ decodeUtf8 b

    get "/header" $ do
        agent <- header "User-Agent"
        maybe (raise "User-Agent header not found!") text agent

    -- Make a request to this URI, then type a line in the terminal, which
    -- will be the response. Using ctrl-c will cause getLine to fail.
    -- This demonstrates that IO exceptions are lifted into ActionM exceptions.
    get "/iofail" $ do
        msg <- liftIO $ liftM fromString getLine
        text msg

{- If you don't want to use Warp as your webserver,
   you can use any WAI handler.

import Network.Wai.Handler.FastCGI (run)

main = do
    myApp <- scottyApp $ do
        get "/" $ text "hello world"

    run myApp
-}

