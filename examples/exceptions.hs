{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
{-# language DeriveAnyClass #-}
{-# language LambdaCase #-}
module Main (main) where

import Control.Exception (Exception(..))
import Control.Monad.IO.Class

import Data.String (fromString)
import Data.Typeable

import Network.HTTP.Types
import Network.Wai.Middleware.RequestLogger

import Prelude ()
import Prelude.Compat

import System.Random

import Web.Scotty.Trans

-- | A custom exception type.
data Except = Forbidden | NotFound Int | StringEx String
    deriving (Show, Eq, Typeable, Exception)

-- | User-defined exceptions should have an associated Handler:
handleEx :: MonadIO m => ErrorHandler m
handleEx = Handler $ \case
  Forbidden -> do
    status status403
    html "<h1>Scotty Says No</h1>"
  NotFound i -> do
    status status404
    html $ fromString $ "<h1>Can't find " ++ show i ++ ".</h1>"
  StringEx s -> do
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
        v <- captureParam "val"
        _ <- if even v then throw Forbidden else throw (NotFound v)
        text "this will never be reached"

    get "/random" $ do
        rBool <- liftIO randomIO
        i <- liftIO randomIO
        let catchOne Forbidden = html "<h1>Forbidden was randomly thrown, but we caught it."
            catchOne other     = throw other
        throw (if rBool then Forbidden else NotFound i) `rescue` catchOne
