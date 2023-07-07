{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Main (main) where

import Control.Monad.IO.Class

import Data.String (fromString)

import Network.HTTP.Types
import Network.Wai.Middleware.RequestLogger

import Prelude ()
import Prelude.Compat

import System.Random

import Web.Scotty.Trans

-- Define a custom exception type.
data Except = Forbidden | NotFound Int | StringEx String
    deriving (Show, Eq)

-- The type must be an instance of 'ScottyError'.
-- 'ScottyError' is essentially a combination of 'Error' and 'Show'.
instance ScottyError Except where
    stringError = StringEx
    showError = fromString . show

-- Handler for uncaught exceptions.
handleEx :: Monad m => Except -> ActionT Except m ()
handleEx Forbidden    = do
    status status403
    html "<h1>Scotty Says No</h1>"
handleEx (NotFound i) = do
    status status404
    html $ fromString $ "<h1>Can't find " ++ show i ++ ".</h1>"
handleEx (StringEx s) = do
    status status500
    html $ fromString $ "<h1>" ++ s ++ "</h1>"

main :: IO ()
main = scottyT 3000 id $ do -- note, we aren't using any additional transformer layers
                            -- so we can just use 'id' for the runner.
    middleware logStdoutDev

    defaultHandler handleEx    -- define what to do with uncaught exceptions

    get "/" $ do
        html $ mconcat ["<a href=\"/switch/1\">Option 1 (Not Found)</a>"
                       ,"<br/>"
                       ,"<a href=\"/switch/2\">Option 2 (Forbidden)</a>"
                       ,"<br/>"
                       ,"<a href=\"/random\">Option 3 (Random)</a>"
                       ]

    get "/switch/:val" $ do
        v <- param "val"
        _ <- if even v then raise Forbidden else raise (NotFound v)
        text "this will never be reached"

    get "/random" $ do
        rBool <- liftIO randomIO
        i <- liftIO randomIO
        let catchOne Forbidden = html "<h1>Forbidden was randomly thrown, but we caught it."
            catchOne other     = raise other
        raise (if rBool then Forbidden else NotFound i) `rescue` catchOne
