{-# LANGUAGE OverloadedStrings, RankNTypes #-}
-- | It should be noted that most of the code snippets below depend on the
-- OverloadedStrings language pragma.
--
-- Scotty is set up by default for development mode. For production servers,
-- you will likely want to modify 'Trans.settings' and the 'defaultHandler'. See
-- the comments on each of these functions for more information.
--
-- Please refer to the @examples@ directory and the @spec@ test suite for concrete use cases, e.g. constructing responses, exception handling and useful implementation details.
module Web.Scotty
    ( -- * running 'scotty' servers
      scotty
    , scottyOpts
    , Options(..), defaultOptions
      -- ** scotty-to-WAI
    , scottyApp, scottySocket
      -- * Defining Middleware and Routes
      --
      -- | 'Middleware' and routes are run in the order in which they
      -- are defined. All middleware is run first, followed by the first
      -- route that matches. If no route matches, a 404 response is given.
    , middleware, get, post, put, delete, patch, options, addroute, matchAny, notFound, nested, setMaxRequestBodySize
      -- ** Route Patterns
    , capture, regex, function, literal
      -- ** Accessing the Request, Captures, and Query Parameters
    , request, header, headers, body, bodyReader
    , param, params
    , pathParam, captureParam, formParam, queryParam
    , pathParamMaybe, captureParamMaybe, formParamMaybe, queryParamMaybe
    , pathParams, captureParams, formParams, queryParams
    , jsonData, files
      -- ** Modifying the Response and Redirecting
    , status, addHeader, setHeader, redirect
      -- ** Setting Response Body
      --
      -- | Note: only one of these should be present in any given route
      -- definition, as they completely replace the current 'Response' body.
    , text, html, file, json, stream, raw
      -- ** Accessing the fields of the Response
    , getResponseHeaders, getResponseStatus, getResponseContent
      -- ** Exceptions
    , raise, raiseStatus, throw, rescue, next, finish, defaultHandler, liftAndCatchIO
    , liftIO, catch
    , StatusError(..)
    , ScottyException(..)
      -- * Parsing Parameters
    , Param, Trans.Parsable(..), Trans.readEither
      -- * Types
    , ScottyM, ActionM, RoutePattern, File, Content(..), Kilobytes, ErrorHandler, Handler(..)
    , ScottyState, defaultScottyState
    ) where

import qualified Web.Scotty.Trans as Trans

import qualified Control.Exception          as E
import Control.Monad.IO.Class
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Text.Lazy (Text, toStrict)

import Network.HTTP.Types (Status, StdMethod, ResponseHeaders)
import Network.Socket (Socket)
import Network.Wai (Application, Middleware, Request, StreamingBody)
import Network.Wai.Handler.Warp (Port)

import Web.Scotty.Internal.Types (ScottyT, ActionT, ErrorHandler, Param, RoutePattern, Options, defaultOptions, File, Kilobytes, ScottyState, defaultScottyState, ScottyException, StatusError(..), Content(..))
import UnliftIO.Exception (Handler(..), catch)

{- $setup
>>> :{
import Control.Monad.IO.Class (MonadIO(..))
import qualified Network.HTTP.Client as H
import qualified Network.HTTP.Types as H
import qualified Network.Wai as W (httpVersion)
import qualified Data.ByteString.Lazy.Char8 as LBS (unpack)
import qualified Data.Text as T (pack)
import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Exception (bracket)
import qualified Web.Scotty as S (ScottyM, scottyOpts, get, text, regex, pathParam, Options(..), defaultOptions)
-- | GET an HTTP path
curl :: MonadIO m =>
        String -- ^ path
     -> m String -- ^ response body
curl path = liftIO $ do
  req0 <- H.parseRequest path
  let req = req0 { H.method = "GET"}
  mgr <- H.newManager H.defaultManagerSettings
  (LBS.unpack . H.responseBody) <$> H.httpLbs req mgr
-- | Fork a process, run a Scotty server in it and run an action while the server is running. Kills the scotty thread once the inner action is done.
withScotty :: S.ScottyM ()
           -> IO a -- ^ inner action, e.g. 'curl "localhost:3000/"'
           -> IO a
withScotty serv act = bracket (forkIO $ S.scottyOpts (S.defaultOptions{ S.verbose = 0 }) serv) killThread (\_ -> act)
:}
-}

type ScottyM = ScottyT IO
type ActionM = ActionT IO

-- | Run a scotty application using the warp server.
scotty :: Port -> ScottyM () -> IO ()
scotty p = Trans.scottyT p id

-- | Run a scotty application using the warp server, passing extra options.
scottyOpts :: Options -> ScottyM () -> IO ()
scottyOpts opts = Trans.scottyOptsT opts id

-- | Run a scotty application using the warp server, passing extra options,
-- and listening on the provided socket. This allows the user to provide, for
-- example, a Unix named socket, which can be used when reverse HTTP proxying
-- into your application.
scottySocket :: Options -> Socket -> ScottyM () -> IO ()
scottySocket opts sock = Trans.scottySocketT opts sock id

-- | Turn a scotty application into a WAI 'Application', which can be
-- run with any WAI handler.
scottyApp :: ScottyM () -> IO Application
scottyApp = Trans.scottyAppT id

-- | Global handler for user-defined exceptions.
defaultHandler :: ErrorHandler IO -> ScottyM ()
defaultHandler = Trans.defaultHandler

-- | Use given middleware. Middleware is nested such that the first declared
-- is the outermost middleware (it has first dibs on the request and last action
-- on the response). Every middleware is run on each request.
middleware :: Middleware -> ScottyM ()
middleware = Trans.middleware

-- | Nest a whole WAI application inside a Scotty handler.
-- Note: You will want to ensure that this route fully handles the response,
-- as there is no easy delegation as per normal Scotty actions.
-- Also, you will have to carefully ensure that you are expecting the correct routes,
-- this could require stripping the current prefix, or adding the prefix to your
-- application's handlers if it depends on them. One potential use-case for this
-- is hosting a web-socket handler under a specific route.
nested :: Application -> ActionM ()
nested = Trans.nested

-- | Set global size limit for the request body. Requests with body size exceeding the limit will not be
-- processed and an HTTP response 413 will be returned to the client. Size limit needs to be greater than 0, 
-- otherwise the application will terminate on start. 
setMaxRequestBodySize :: Kilobytes -> ScottyM ()
setMaxRequestBodySize = Trans.setMaxRequestBodySize

-- | Throw a "500 Server Error" 'StatusError', which can be caught with 'catch'.
--
-- Uncaught exceptions turn into HTTP 500 responses.
raise :: Text -> ActionM a
raise = Trans.raise
{-# DEPRECATED raise "Throw an exception instead" #-}

-- | Throw a 'StatusError' exception that has an associated HTTP error code and can be caught with 'catch'.
--
-- Uncaught exceptions turn into HTTP responses corresponding to the given status.
raiseStatus :: Status -> Text -> ActionM a
raiseStatus = Trans.raiseStatus
{-# DEPRECATED raiseStatus "Use status, text, and finish instead" #-}

-- | Throw an exception which can be caught within the scope of the current Action with 'catch'.
--
-- If the exception is not caught locally, another option is to implement a global 'Handler' (with 'defaultHandler') that defines its interpretation and a translation to HTTP error codes.
--
-- Uncaught exceptions turn into HTTP 500 responses.
throw :: (E.Exception e) => e -> ActionM a
throw = Trans.throw

-- | Abort execution of this action and continue pattern matching routes.
-- Like an exception, any code after 'next' is not executed.
--
-- NB : Internally, this is implemented with an exception that can only be
-- caught by the library, but not by the user.
--
-- As an example, these two routes overlap. The only way the second one will
-- ever run is if the first one calls 'next'.
--
-- > get "/foo/:bar" $ do
-- >   w :: Text <- pathParam "bar"
-- >   unless (w == "special") next
-- >   text "You made a request to /foo/special"
-- >
-- > get "/foo/:baz" $ do
-- >   w <- pathParam "baz"
-- >   text $ "You made a request to: " <> w
next :: ActionM ()
next = Trans.next

-- | Abort execution of this action. Like an exception, any code after 'finish'
-- is not executed.
--
-- As an example only requests to @\/foo\/special@ will include in the response
-- content the text message.
--
-- > get "/foo/:bar" $ do
-- >   w :: Text <- pathParam "bar"
-- >   unless (w == "special") finish
-- >   text "You made a request to /foo/special"
--
-- /Since: 0.10.3/
finish :: ActionM a
finish = Trans.finish

-- | Catch an exception e.g. a 'StatusError' or a user-defined exception.
--
-- > raise JustKidding `catch` (\msg -> text msg)
rescue :: E.Exception e => ActionM a -> (e -> ActionM a) -> ActionM a
rescue = Trans.rescue
{-# DEPRECATED rescue "Use catch instead" #-}

-- | Like 'liftIO', but catch any IO exceptions and turn them into Scotty exceptions.
liftAndCatchIO :: IO a -> ActionM a
liftAndCatchIO = Trans.liftAndCatchIO
{-# DEPRECATED liftAndCatchIO "Use liftIO instead" #-}

-- | Redirect to given URL. Like throwing an uncatchable exception. Any code after the call to redirect
-- will not be run.
--
-- > redirect "http://www.google.com"
--
-- OR
--
-- > redirect "/foo/bar"
redirect :: Text -> ActionM a
redirect = Trans.redirect

-- | Get the 'Request' object.
request :: ActionM Request
request = Trans.request

-- | Get list of uploaded files.
files :: ActionM [File]
files = Trans.files

-- | Get a request header. Header name is case-insensitive.
header :: Text -> ActionM (Maybe Text)
header = Trans.header

-- | Get all the request headers. Header names are case-insensitive.
headers :: ActionM [(Text, Text)]
headers = Trans.headers

-- | Get the request body.
body :: ActionM ByteString
body = Trans.body

-- | Get an IO action that reads body chunks
--
-- * This is incompatible with 'body' since 'body' consumes all chunks.
bodyReader :: ActionM (IO BS.ByteString)
bodyReader = Trans.bodyReader

-- | Parse the request body as a JSON object and return it. Raises an exception if parse is unsuccessful.
jsonData :: FromJSON a => ActionM a
jsonData = Trans.jsonData

-- | Get a parameter. First looks in captures, then form data, then query parameters.
--
-- * Raises an exception which can be caught by 'catch' if parameter is not found.
--
-- * If parameter is found, but 'parseParam' fails to parse to the correct type, 'next' is called.
--   This means captures are somewhat typed, in that a route won't match if a correctly typed
--   capture cannot be parsed.
param :: Trans.Parsable a => Text -> ActionM a
param = Trans.param . toStrict
{-# DEPRECATED param "(#204) Not a good idea to treat all parameters identically. Use pathParam, formParam and queryParam instead. "#-}

-- | Synonym for 'pathParam'
captureParam :: Trans.Parsable a => Text -> ActionM a
captureParam = Trans.captureParam . toStrict

-- | Get a path parameter.
--
-- * Raises an exception which can be caught by 'catch' if parameter is not found. If the exception is not caught, scotty will return a HTTP error code 500 ("Internal Server Error") to the client.
--
-- * If the parameter is found, but 'parseParam' fails to parse to the correct type, 'next' is called.
--
-- /Since: 0.21/
pathParam :: Trans.Parsable a => Text -> ActionM a
pathParam = Trans.pathParam . toStrict

-- | Get a form parameter.
--
-- * Raises an exception which can be caught by 'catch' if parameter is not found. If the exception is not caught, scotty will return a HTTP error code 400 ("Bad Request") to the client.
--
-- * This function raises a code 400 also if the parameter is found, but 'parseParam' fails to parse to the correct type.
--
-- /Since: 0.20/
formParam :: Trans.Parsable a => Text -> ActionM a
formParam = Trans.formParam . toStrict

-- | Get a query parameter.
--
-- * Raises an exception which can be caught by 'catch' if parameter is not found. If the exception is not caught, scotty will return a HTTP error code 400 ("Bad Request") to the client.
--
-- * This function raises a code 400 also if the parameter is found, but 'parseParam' fails to parse to the correct type.
--
-- /Since: 0.20/
queryParam :: Trans.Parsable a => Text -> ActionM a
queryParam = Trans.queryParam . toStrict


-- | Look up a path parameter. Returns 'Nothing' if the parameter is not found or cannot be parsed at the right type.
--
-- NB : Doesn't throw exceptions. In particular, route pattern matching will not continue, so developers
-- must 'raiseStatus' or 'throw' to signal something went wrong.
--
-- /Since: FIXME/
pathParamMaybe :: (Trans.Parsable a) => Text -> ActionM (Maybe a)
pathParamMaybe = Trans.pathParamMaybe . toStrict

-- | Synonym for 'pathParamMaybe'
captureParamMaybe :: (Trans.Parsable a) => Text -> ActionM (Maybe a)
captureParamMaybe = Trans.pathParamMaybe . toStrict

-- | Look up a form parameter. Returns 'Nothing' if the parameter is not found or cannot be parsed at the right type.
--
-- NB : Doesn't throw exceptions, so developers must 'raiseStatus' or 'throw' to signal something went wrong.
--
-- /Since: FIXME/
formParamMaybe :: (Trans.Parsable a) => Text -> ActionM (Maybe a)
formParamMaybe = Trans.formParamMaybe . toStrict

-- | Look up a query parameter. Returns 'Nothing' if the parameter is not found or cannot be parsed at the right type.
--
-- NB : Doesn't throw exceptions, so developers must 'raiseStatus' or 'throw' to signal something went wrong.
--
-- /Since: FIXME/
queryParamMaybe :: (Trans.Parsable a) => Text -> ActionM (Maybe a)
queryParamMaybe = Trans.queryParamMaybe . toStrict




-- | Get all parameters from path, form and query (in that order).
params :: ActionM [Param]
params = Trans.params
{-# DEPRECATED params "(#204) Not a good idea to treat all parameters identically. Use pathParams, formParams and queryParams instead. "#-}

-- | Synonym for 'pathParams'
captureParams :: ActionM [Param]
captureParams = Trans.captureParams
-- | Get path parameters
pathParams :: ActionM [Param]
pathParams = Trans.pathParams
-- | Get form parameters
formParams :: ActionM [Param]
formParams = Trans.formParams
-- | Get query parameters
queryParams :: ActionM [Param]
queryParams = Trans.queryParams


-- | Set the HTTP response status. Default is 200.
status :: Status -> ActionM ()
status = Trans.status

-- | Add to the response headers. Header names are case-insensitive.
addHeader :: Text -> Text -> ActionM ()
addHeader = Trans.addHeader

-- | Set one of the response headers. Will override any previously set value for that header.
-- Header names are case-insensitive.
setHeader :: Text -> Text -> ActionM ()
setHeader = Trans.setHeader

-- | Set the body of the response to the given 'Text' value. Also sets \"Content-Type\"
-- header to \"text/plain; charset=utf-8\" if it has not already been set.
text :: Text -> ActionM ()
text = Trans.text

-- | Set the body of the response to the given 'Text' value. Also sets \"Content-Type\"
-- header to \"text/html; charset=utf-8\" if it has not already been set.
html :: Text -> ActionM ()
html = Trans.html

-- | Send a file as the response. Doesn't set the \"Content-Type\" header, so you probably
-- want to do that on your own with 'setHeader'.
file :: FilePath -> ActionM ()
file = Trans.file

-- | Set the body of the response to the JSON encoding of the given value. Also sets \"Content-Type\"
-- header to \"application/json; charset=utf-8\" if it has not already been set.
json :: ToJSON a => a -> ActionM ()
json = Trans.json

-- | Set the body of the response to a StreamingBody. Doesn't set the
-- \"Content-Type\" header, so you probably want to do that on your
-- own with 'setHeader'.
stream :: StreamingBody -> ActionM ()
stream = Trans.stream

-- | Set the body of the response to the given 'BL.ByteString' value. Doesn't set the
-- \"Content-Type\" header, so you probably want to do that on your own with 'setHeader'.
raw :: ByteString -> ActionM ()
raw = Trans.raw


-- | Access the HTTP 'Status' of the Response
getResponseStatus :: ActionM Status
getResponseStatus = Trans.getResponseStatus
-- | Access the HTTP headers of the Response
getResponseHeaders :: ActionM ResponseHeaders
getResponseHeaders = Trans.getResponseHeaders
-- | Access the content of the Response
getResponseContent :: ActionM Content
getResponseContent = Trans.getResponseContent


-- | get = 'addroute' 'GET'
get :: RoutePattern -> ActionM () -> ScottyM ()
get = Trans.get

-- | post = 'addroute' 'POST'
post :: RoutePattern -> ActionM () -> ScottyM ()
post = Trans.post

-- | put = 'addroute' 'PUT'
put :: RoutePattern -> ActionM () -> ScottyM ()
put = Trans.put

-- | delete = 'addroute' 'DELETE'
delete :: RoutePattern -> ActionM () -> ScottyM ()
delete = Trans.delete

-- | patch = 'addroute' 'PATCH'
patch :: RoutePattern -> ActionM () -> ScottyM ()
patch = Trans.patch

-- | options = 'addroute' 'OPTIONS'
options :: RoutePattern -> ActionM () -> ScottyM ()
options = Trans.options

-- | Add a route that matches regardless of the HTTP verb.
matchAny :: RoutePattern -> ActionM () -> ScottyM ()
matchAny = Trans.matchAny

-- | Specify an action to take if nothing else is found. Note: this _always_ matches,
-- so should generally be the last route specified.
notFound :: ActionM () -> ScottyM ()
notFound = Trans.notFound

{- | Define a route with a 'StdMethod', a route pattern representing the path spec,
and an 'Action' which may modify the response.

> get "/" $ text "beam me up!"

The path spec can include values starting with a colon, which are interpreted
as /captures/. These are parameters that can be looked up with 'pathParam'.

>>> :{
let server = S.get "/foo/:bar" (S.pathParam "bar" >>= S.text)
 in do
      withScotty server $ curl "http://localhost:3000/foo/something"
:}
"something"
-}
addroute :: StdMethod -> RoutePattern -> ActionM () -> ScottyM ()
addroute = Trans.addroute


{- | Match requests using a regular expression.
Named captures are not yet supported.

>>> :{
let server = S.get (S.regex "^/f(.*)r$") $ do
                cap <- S.pathParam "1"
                S.text cap
 in do
      withScotty server $ curl "http://localhost:3000/foo/bar"
:}
"oo/ba"
-}
regex :: String -> RoutePattern
regex = Trans.regex

-- | Standard Sinatra-style route. Named captures are prepended with colons.
--   This is the default route type generated by OverloadedString routes. i.e.
--
-- > get (capture "/foo/:bar") $ ...
--
--   and
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > ...
-- > get "/foo/:bar" $ ...
--
--   are equivalent.
capture :: String -> RoutePattern
capture = Trans.capture


{- | Build a route based on a function which can match using the entire 'Request' object.
'Nothing' indicates the route does not match. A 'Just' value indicates
a successful match, optionally returning a list of key-value pairs accessible by 'param'.

>>> :{
let server = S.get (function $ \req -> Just [("version", T.pack $ show $ W.httpVersion req)]) $ do
                v <- S.pathParam "version"
                S.text v
 in do
      withScotty server $ curl "http://localhost:3000/"
:}
"HTTP/1.1"
-}
function :: (Request -> Maybe [Param]) -> RoutePattern
function = Trans.function

-- | Build a route that requires the requested path match exactly, without captures.
literal :: String -> RoutePattern
literal = Trans.literal




