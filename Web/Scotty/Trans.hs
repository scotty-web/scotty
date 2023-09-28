{-# LANGUAGE OverloadedStrings, RankNTypes #-}
-- | It should be noted that most of the code snippets below depend on the
-- OverloadedStrings language pragma.
--
-- The functions in this module allow an arbitrary monad to be embedded
-- in Scotty's monad transformer stack in order that Scotty be combined
-- with other DSLs.
--
-- Scotty is set up by default for development mode. For production servers,
-- you will likely want to modify 'settings' and the 'defaultHandler'. See
-- the comments on each of these functions for more information.
module Web.Scotty.Trans
    ( -- * scotty-to-WAI
      scottyT, scottyAppT, scottyOptsT, scottySocketT, Options(..)
      -- * Defining Middleware and Routes
      --
      -- | 'Middleware' and routes are run in the order in which they
      -- are defined. All middleware is run first, followed by the first
      -- route that matches. If no route matches, a 404 response is given.
    , middleware, get, post, put, delete, patch, options, addroute, matchAny, notFound, setMaxRequestBodySize
      -- ** Route Patterns
    , capture, regex, function, literal
      -- ** Accessing the Request, Captures, and Query Parameters
    , request, header, headers, body, bodyReader
    , param, params
    , captureParam, formParam, queryParam
    , captureParams, formParams, queryParams
    , jsonData, files
      -- ** Modifying the Response and Redirecting
    , status, addHeader, setHeader, redirect
      -- ** Setting Response Body
      --
      -- | Note: only one of these should be present in any given route
      -- definition, as they completely replace the current 'Response' body.
    , text, html, file, json, stream, raw, nested
      -- ** Exceptions
    , raise, raiseStatus, rescue, next, finish, defaultHandler, ScottyError(..), liftAndCatchIO
      -- * Parsing Parameters
    , Param, Parsable(..), readEither
      -- * Types
    , RoutePattern, File, Kilobytes
      -- * Monad Transformers
    , ScottyT, ActionT
    ) where

import Blaze.ByteString.Builder (fromByteString)

import Control.Exception (assert)
import Control.Monad (when)
import Control.Monad.State.Strict (execState, modify)
import Control.Monad.IO.Class

-- import Data.Default.Class (def)

import Network.HTTP.Types (status404, status500)
import Network.Socket (Socket)
import Network.Wai
import Network.Wai.Handler.Warp (Port, runSettings, runSettingsSocket, setPort, getPort)

import Web.Scotty.Action
import Web.Scotty.Route
import Web.Scotty.Internal.Types hiding (Application, Middleware)
import Web.Scotty.Util (socketDescription)
import qualified Web.Scotty.Internal.Types as Scotty
import Web.Scotty.Body (newBodyInfo)

-- | Run a scotty application using the warp server.
-- NB: scotty p === scottyT p id
scottyT :: (Monad m, MonadIO n)
        => Port
        -> (m Response -> IO Response) -- ^ Run monad 'm' into 'IO', called at each action.
        -> ScottyT e m ()
        -> n ()
scottyT p = scottyOptsT $ defaultOptions { settings = setPort p (settings defaultOptions) }

-- | Run a scotty application using the warp server, passing extra options.
-- NB: scottyOpts opts === scottyOptsT opts id
scottyOptsT :: (Monad m, MonadIO n)
            => Options
            -> (m Response -> IO Response) -- ^ Run monad 'm' into 'IO', called at each action.
            -> ScottyT e m ()
            -> n ()
scottyOptsT opts runActionToIO s = do
    when (verbose opts > 0) $
        liftIO $ putStrLn $ "Setting phasers to stun... (port " ++ show (getPort (settings opts)) ++ ") (ctrl-c to quit)"
    liftIO . runSettings (settings opts) =<< scottyAppT runActionToIO s

-- | Run a scotty application using the warp server, passing extra options, and
-- listening on the provided socket.
-- NB: scottySocket opts sock === scottySocketT opts sock id
scottySocketT :: (Monad m, MonadIO n)
              => Options
              -> Socket
              -> (m Response -> IO Response)
              -> ScottyT e m ()
              -> n ()
scottySocketT opts sock runActionToIO s = do
    when (verbose opts > 0) $ do
        d <- liftIO $ socketDescription sock
        liftIO $ putStrLn $ "Setting phasers to stun... (" ++ d ++ ") (ctrl-c to quit)"
    liftIO . runSettingsSocket (settings opts) sock =<< scottyAppT runActionToIO s

-- | Turn a scotty application into a WAI 'Application', which can be
-- run with any WAI handler.
-- NB: scottyApp === scottyAppT id
scottyAppT :: (Monad m, Monad n)
           => (m Response -> IO Response) -- ^ Run monad 'm' into 'IO', called at each action.
           -> ScottyT e m ()
           -> n Application
scottyAppT runActionToIO defs = do
    let s = execState (runS defs) defaultScottyState
    let rapp req callback = do
          bodyInfo <- newBodyInfo req
          runActionToIO (applyAll notFoundApp ([midd bodyInfo | midd <- routes s]) req) >>= callback
    return $ applyAll rapp (middlewares s)

applyAll :: Foldable t => a -> t (a -> a) -> a
applyAll = foldl (flip ($))

notFoundApp :: Monad m => Scotty.Application m
notFoundApp _ = return $ responseBuilder status404 [("Content-Type","text/html")]
                       $ fromByteString "<h1>404: File Not Found!</h1>"

-- | Global handler for uncaught exceptions.
--
-- Uncaught exceptions normally become 500 responses.
-- You can use this to selectively override that behavior.
--
-- Note: IO exceptions are lifted into 'ScottyError's by 'stringError'.
-- This has security implications, so you probably want to provide your
-- own defaultHandler in production which does not send out the error
-- strings as 500 responses.
defaultHandler :: (Monad m) => (AErr -> ActionT e m ()) -> ScottyT e m ()
defaultHandler f = ScottyT $ modify $ addHandler $ Just (\e -> status status500 >> f e)

-- | Use given middleware. Middleware is nested such that the first declared
-- is the outermost middleware (it has first dibs on the request and last action
-- on the response). Every middleware is run on each request.
middleware :: Middleware -> ScottyT e m ()
middleware = ScottyT . modify . addMiddleware

-- | Set global size limit for the request body. Requests with body size exceeding the limit will not be
-- processed and an HTTP response 413 will be returned to the client. Size limit needs to be greater than 0, 
-- otherwise the application will terminate on start. 
setMaxRequestBodySize :: Kilobytes -> ScottyT e m ()
setMaxRequestBodySize i = assert (i > 0) $ ScottyT . modify . updateMaxRequestBodySize $ defaultRouteOptions { maxRequestBodySize = Just i } 
