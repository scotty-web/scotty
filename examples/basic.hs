{-# LANGUAGE OverloadedStrings #-}
{-# language DeriveAnyClass #-}
{-# language ScopedTypeVariables #-}
module Main (main) where

import Web.Scotty

import Network.Wai.Middleware.RequestLogger -- install wai-extra if you don't have this

import Control.Exception (Exception(..))
import Control.Monad
import Control.Monad.Trans
import System.Random (newStdGen, randomRs)

import Network.HTTP.Types (status302)

import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.String (fromString)
import Data.Typeable (Typeable)
import Prelude ()
import Prelude.Compat

data Err = Boom | UserAgentNotFound | NeverReached deriving (Show, Typeable, Exception)


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

    -- Using a parameter in the query string. Since it has
    -- not been given, a 500 page is generated.
    get "/foo" $ do
        v <- captureParam "fooparam"
        html $ mconcat ["<h1>", v, "</h1>"]

    -- An uncaught error becomes a 500 page.
    get "/raise" $ raise Boom

    -- You can set status and headers directly.
    get "/redirect-custom" $ do
        status status302
        setHeader "Location" "http://www.google.com"
        -- note first arg to header is NOT case-sensitive

    -- redirects preempt execution
    get "/redirect" $ do
        void $ redirect "http://www.google.com"
        raise NeverReached

    -- Of course you can catch your own errors.
    get "/rescue" $ do
        (do void $ raise Boom; redirect "http://www.we-never-go-here.com")
        `rescue` (\(e :: Err) -> text $ "we recovered from " `mappend` pack (show e))

    -- Parts of the URL that start with a colon match
    -- any string, and capture that value as a parameter.
    -- URL captures take precedence over query string parameters.
    get "/foo/:bar/required" $ do
        v <- captureParam "bar"
        html $ mconcat ["<h1>", v, "</h1>"]

    -- Files are streamed directly to the client.
    get "/404" $ file "404.html"

    -- 'next' stops execution of the current action and keeps pattern matching routes.
    get "/random" $ do
        void next
        redirect "http://www.we-never-go-here.com"

    -- You can do IO with liftIO, and you can return JSON content.
    get "/random" $ do
        g <- liftIO newStdGen
        json $ take 20 $ randomRs (1::Int,100) g

    get "/ints/:is" $ do
        is <- captureParam "is"
        json $ [(1::Int)..10] ++ is

    get "/setbody" $ do
        html $ mconcat ["<form method=POST action=\"readbody\">"
                       ,"<input type=text name=something>"
                       ,"<input type=submit>"
                       ,"</form>"
                       ]

    -- Read and decode the request body as UTF-8
    post "/readbody" $ do
        b <- body
        text $ decodeUtf8 b

    -- Look up a request header
    get "/header" $ do
        agent <- header "User-Agent"
        maybe (raise UserAgentNotFound) text agent

    -- Make a request to this URI, then type a line in the terminal, which
    -- will be the response. Using ctrl-c will cause getLine to fail.
    -- This demonstrates that IO exceptions are lifted into ActionM exceptions.
    --
    -- (#310) we don't catch async exceptions, so ctrl-c just exits the program
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

