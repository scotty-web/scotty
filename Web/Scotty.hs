{-# LANGUAGE OverloadedStrings, RankNTypes #-}
-- | It should be noted that most of the code snippets below depend on the
-- OverloadedStrings language pragma.
module Web.Scotty
    ( -- * scotty-to-WAI
      scotty, scottyApp, scottyOpts, Options(..)
      -- * Defining Middleware and Routes
      --
      -- | 'Middleware' and routes are run in the order in which they
      -- are defined. All middleware is run first, followed by the first
      -- route that matches. If no route matches, a 404 response is given.
    , middleware, get, post, getpost, put, delete, addroute, matchAny, notFound
      -- ** Route Patterns
    , capture, regex, function, literal
      -- ** Accessing the Request, Captures, and Query Parameters
    , request, reqHeader, body, param, params, jsonData, files
      -- ** Modifying the Response and Redirecting
    , status, header, redirect
      -- ** Setting Response Body
      --
      -- | Note: only one of these should be present in any given route
      -- definition, as they completely replace the current 'Response' body.
    , text, html, file, json, source
      -- ** Exceptions
    , raise, rescue, next
      -- * Parsing Parameters
    , Param, Parsable(..), readEither
      -- * Types
    , ScottyM, ActionM, RoutePattern, File
      -- * Monad Transformers
    , ScottyT, ActionT
    ) where

import Blaze.ByteString.Builder (fromByteString)

import Control.Monad (when)
import Control.Monad.State (execStateT, modify)
import Control.Monad.Morph (hoist)

import Data.Default (def)

import Network.HTTP.Types (status404)
import Network.Wai
import Network.Wai.Handler.Warp (Port, runSettings, settingsPort)

import Web.Scotty.Action
import Web.Scotty.Route
import Web.Scotty.Types hiding (Application, Middleware)
import qualified Web.Scotty.Types as Scotty

-- | Run a scotty application using the warp server.
scotty :: Port -> ScottyM () -> IO ()
scotty p = scottyOpts $ def { settings = (settings def) { settingsPort = p } }

-- | Run a scotty application using the warp server, passing extra options.
scottyOpts :: Options -> ScottyM () -> IO ()
scottyOpts opts s = do
    when (verbose opts > 0) $
        putStrLn $ "Setting phasers to stun... (port " ++ show (settingsPort (settings opts)) ++ ") (ctrl-c to quit)"
    runSettings (settings opts) =<< scottyApp id id s

-- | Turn a scotty application into a WAI 'Application', which can be
-- run with any WAI handler.
scottyApp :: (Monad m, Monad n)
          => (forall a. m a -> n a) -- run monad m into monad n, called once at ScottyT level
          -> (forall a. m a -> IO a) -- run monad m into IO, called at each action
          -> ScottyT m ()
          -> n Application
scottyApp runM runToIO defs = do
    s <- runM $ execStateT (runS defs) def
    return $ hoist runToIO . foldl (flip ($)) notFoundApp (routes s ++ middlewares s)

notFoundApp :: Monad m => Scotty.Application m
notFoundApp _ = return $ ResponseBuilder status404 [("Content-Type","text/html")]
                       $ fromByteString "<h1>404: File Not Found!</h1>"

-- | Use given middleware. Middleware is nested such that the first declared
-- is the outermost middleware (it has first dibs on the request and last action
-- on the response). Every middleware is run on each request.
middleware :: Monad m => Scotty.Middleware m -> ScottyT m ()
middleware = modify . addMiddleware
