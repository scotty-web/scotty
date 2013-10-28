{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
-- An example of embedding a custom monad into
-- Scotty's transformer stack, using ErrorT to provide
-- custom exceptions and a centralized exception handler.
module Main where

import Control.Applicative
import Control.Monad.Error

import Data.ByteString.Lazy hiding (pack)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Monoid

import Network.HTTP.Types
import Network.Wai.Middleware.RequestLogger
import Network.Wai

import Web.Scotty.Trans

newtype ExM a = ExM { runExM :: ErrorT Except IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadError Except)

data Except = Forbidden | NotFound Int | Other ByteString
    deriving (Show, Eq)

instance Error Except where
    strMsg = Other . pack

handleEx :: Except -> IO Response
handleEx Forbidden    = return $ plainResponse status403 "Scotty says no."
handleEx (NotFound i) = return $ plainResponse status404 (pack $ "Can't find " ++ show i ++ ".")
handleEx (Other bs)   = return $ plainResponse status500 bs

plainResponse :: Status -> ByteString -> Response
plainResponse st bs = responseLBS st [("Content-type","text/plain")] bs

-- Scotty's monads are layered on top of our custom monad.
-- We define this helper to put our exceptions in the right layer.
throwEx :: MonadTrans t => Except -> t ExM ()
throwEx = lift . throwError

main :: IO ()
main = do
    let runM m = do
            r <- runErrorT (runExM m) 
            either (\ ex -> fail $ "exception at startup: " ++ show ex) return r
        -- 'runActionToIO' is called once per action.
        runActionToIO m = runErrorT (runExM m) >>= either handleEx return

    scottyT 3000 runM runActionToIO $ do
        middleware logStdoutDev
        get "/" $ do
            html $ mconcat ["<a href=\"/switch/1\">Option 1 (Not Found)</a>"
                           ,"<br/>"
                           ,"<a href=\"/switch/2\">Option 2 (Forbidden)</a>"
                           ]

        get "/switch/:val" $ do
            v :: Int <- param "val"
            if even v then throwEx Forbidden else throwEx (NotFound v)
            text "this will never be reached"
