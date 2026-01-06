{-# LANGUAGE OverloadedStrings, RankNTypes, NamedFieldPuns #-}
{-# language LambdaCase #-}
-- | It should be noted that most of the code snippets below depend on the
-- OverloadedStrings language pragma.
--
-- The functions in this module allow an arbitrary monad to be embedded
-- in Scotty's monad transformer stack, e.g. for complex endpoint configuration,
-- interacting with databases etc.
--
-- Scotty is set up by default for development mode. For production servers,
-- you will likely want to modify 'settings' and the 'defaultHandler'. See
-- the comments on each of these functions for more information.
--
-- Please refer to the @examples@ directory and the @spec@ test suite for concrete use cases, e.g. constructing responses, exception handling and useful implementation details.
module Web.Scotty.Trans
    ( -- * Running 'scotty' servers
      scottyT
    , scottyOptsT
    , scottySocketT
    , Options(..), defaultOptions
      -- ** scotty-to-WAI
    , scottyAppT
      -- * Defining Middleware and Routes
      --
      -- | 'Middleware' and routes are run in the order in which they
      -- are defined. All middleware is run first, followed by the first
      -- route that matches. If no route matches, a 404 response is given.
    , middleware, get, post, put, delete, patch, options, addroute, matchAny, notFound, setMaxRequestBodySize
      -- ** Route Patterns
    , capture, regex, function, literal
      -- ** Accessing the Request and its fields
    , request, Lazy.header, Lazy.headers, body, bodyReader
    , jsonData, formData

      -- ** Accessing Path, Form and Query Parameters
    , pathParam, captureParam, formParam, queryParam
    , pathParamMaybe, captureParamMaybe, formParamMaybe, queryParamMaybe
    , pathParams, captureParams, formParams, queryParams
    -- *** Files
    , files, filesOpts
      -- ** Modifying the Response
    , status, Lazy.addHeader, Lazy.setHeader
      -- ** Redirecting
    , Lazy.redirect, Lazy.redirect300, Lazy.redirect301, Lazy.redirect302, Lazy.redirect303
    , Lazy.redirect304, Lazy.redirect307, Lazy.redirect308
      -- ** Setting Response Body
      --
      -- | Note: only one of these should be present in any given route
      -- definition, as they completely replace the current 'Response' body.
    , Lazy.text, Lazy.html, file, json, stream, raw, nested
      -- ** Accessing the fields of the Response
    , getResponseHeaders, getResponseStatus, getResponseContent
      -- ** Exceptions
    , throw, next, finish, defaultHandler
    , liftIO, catch
    , ScottyException(..)
      -- * Parsing Parameters
    , Param, Parsable(..), readEither
      -- * Types
    , RoutePattern, File, Content(..), Kilobytes, ErrorHandler, Handler(..)
      -- * Monad Transformers
    , ScottyT, ActionT
    , ScottyState, defaultScottyState
    -- ** Functions from Cookie module
    , setSimpleCookie, getCookie, getCookies, deleteCookie, makeSimpleCookie
    -- ** Session Management
    , Session (..), SessionId, SessionJar, createSessionJar,
    createUserSession, createSession, readUserSession,
    readSession, getUserSession, getSession, addSession, deleteSession, maintainSessions
    ) where

import Blaze.ByteString.Builder (fromByteString, fromLazyByteString)
import Blaze.ByteString.Builder.Char8 (fromString)

import Control.Exception (assert)
import Control.Monad (when)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State.Strict (execState, modify)
import Control.Monad.IO.Class

import qualified Data.Aeson as A
import qualified Data.Text as T

import Network.HTTP.Types (status404, status413, status500)
import Network.Socket (Socket)
import qualified Network.Wai as W (Application, Middleware, Response, responseBuilder)
import Network.Wai.Handler.Warp (Port, runSettings, runSettingsSocket, setPort, getPort)

import Web.Scotty.Action
import Web.Scotty.Route
import Web.Scotty.Internal.Types (ScottyT(..), defaultScottyState, Application, RoutePattern, Options(..), defaultOptions, RouteOptions(..), defaultRouteOptions, ErrorHandler, Kilobytes, File, addMiddleware, setHandler, updateMaxRequestBodySize, routes, middlewares, ScottyException(..), ScottyState, defaultScottyState, Content(..))
import Web.Scotty.Trans.Lazy as Lazy
import Web.Scotty.Util (socketDescription)
import Web.Scotty.Body (newBodyInfo)

import UnliftIO.Exception (Handler(..), catch)
import Web.Scotty.Cookie (setSimpleCookie,getCookie,getCookies,deleteCookie,makeSimpleCookie)
import Web.Scotty.Session (Session (..), SessionId, SessionJar, createSessionJar,
    createUserSession, createSession, readUserSession,
    readSession, getUserSession, getSession, addSession, deleteSession, maintainSessions)


-- | Run a scotty application using the warp server.
-- NB: scotty p === scottyT p id
scottyT :: (Monad m, MonadIO n)
        => Port
        -> (m W.Response -> IO W.Response) -- ^ Run monad 'm' into 'IO', called at each action.
        -> ScottyT m ()
        -> n ()
scottyT p = scottyOptsT $ defaultOptions { settings = setPort p (settings defaultOptions) }

-- | Run a scotty application using the warp server, passing extra options.
-- NB: scottyOpts opts === scottyOptsT opts id
scottyOptsT :: (Monad m, MonadIO n)
            => Options
            -> (m W.Response -> IO W.Response) -- ^ Run monad 'm' into 'IO', called at each action.
            -> ScottyT m ()
            -> n ()
scottyOptsT opts runActionToIO s = do
    when (verbose opts > 0) $
        liftIO $ putStrLn $ "Setting phasers to stun... (port " ++ show (getPort (settings opts)) ++ ") (ctrl-c to quit)"
    liftIO . runSettings (settings opts) =<< scottyAppT opts runActionToIO s

-- | Run a scotty application using the warp server, passing extra options, and
-- listening on the provided socket.
-- NB: scottySocket opts sock === scottySocketT opts sock id
scottySocketT :: (Monad m, MonadIO n)
              => Options
              -> Socket
              -> (m W.Response -> IO W.Response)
              -> ScottyT m ()
              -> n ()
scottySocketT opts sock runActionToIO s = do
    when (verbose opts > 0) $ do
        d <- liftIO $ socketDescription sock
        liftIO $ putStrLn $ "Setting phasers to stun... (" ++ d ++ ") (ctrl-c to quit)"
    liftIO . runSettingsSocket (settings opts) sock =<< scottyAppT opts runActionToIO s

-- | Turn a scotty application into a WAI 'Application', which can be
-- run with any WAI handler.
-- NB: scottyApp === scottyAppT id
scottyAppT :: (Monad m, Monad n)
           => Options
           -> (m W.Response -> IO W.Response) -- ^ Run monad 'm' into 'IO', called at each action.
           -> ScottyT m ()
           -> n W.Application
scottyAppT opts runActionToIO defs = do
    let s = execState (runReaderT (runS defs) opts) defaultScottyState
    let rapp req callback = do
          bodyInfo <- newBodyInfo req
          resp <- runActionToIO (applyAll (notFoundApp opts) ([midd bodyInfo | midd <- routes s]) req)
            `catch` unhandledExceptionHandler opts
          callback resp
    return $ applyAll rapp (middlewares s)

-- | Exception handler in charge of 'ScottyException' that's not caught by 'scottyExceptionHandler'
unhandledExceptionHandler :: MonadIO m => Options -> ScottyException -> m W.Response
unhandledExceptionHandler Options{jsonMode} = \case
  RequestTooLarge ->
    if jsonMode
      then return $ W.responseBuilder status413 ctJson $ 
        fromLazyByteString $ A.encode $ A.object
          [ "status" A..= (413 :: Int)
          , "description" A..= ("Request is too big Jim!" :: T.Text)
          ]
      else return $ W.responseBuilder status413 ctText "Request is too big Jim!"
  e ->
    if jsonMode
      then return $ W.responseBuilder status500 ctJson $ 
        fromLazyByteString $ A.encode $ A.object
          [ "status" A..= (500 :: Int)
          , "description" A..= ("Internal Server Error: " <> T.pack (show e))
          ]
      else return $ W.responseBuilder status500 ctText $ "Internal Server Error: " <> fromString (show e)
  where
    ctText = [("Content-Type", "text/plain")]
    ctJson = [("Content-Type", "application/json")]

applyAll :: Foldable t => a -> t (a -> a) -> a
applyAll = foldl (flip ($))

notFoundApp :: Monad m => Options -> Application m
notFoundApp Options{jsonMode} _ =
  if jsonMode
    then return $ W.responseBuilder status404 [("Content-Type","application/json; charset=utf-8")]
                       $ fromLazyByteString $ A.encode $ A.object
                           [ "status" A..= (404 :: Int)
                           , "description" A..= ("File Not Found!" :: T.Text)
                           ]
    else return $ W.responseBuilder status404 [("Content-Type","text/html")]
                       $ fromByteString "<h1>404: File Not Found!</h1>"

-- | Global handler for user-defined exceptions.
defaultHandler :: (Monad m) => ErrorHandler m -> ScottyT m ()
defaultHandler f = ScottyT $ modify $ setHandler $ Just f

-- | Use given middleware. Middleware is nested such that the first declared
-- is the outermost middleware (it has first dibs on the request and last action
-- on the response). Every middleware is run on each request.
middleware :: W.Middleware -> ScottyT m ()
middleware = ScottyT . modify . addMiddleware

-- | Set global size limit for the request body. Requests with body size exceeding the limit will not be
-- processed and an HTTP response 413 will be returned to the client. Size limit needs to be greater than 0,
-- otherwise the application will terminate on start.
setMaxRequestBodySize :: Kilobytes -- ^ Request size limit
                      -> ScottyT m ()
setMaxRequestBodySize i = assert (i > 0) $ ScottyT . modify . updateMaxRequestBodySize $ defaultRouteOptions { maxRequestBodySize = Just i }

