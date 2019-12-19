{-# LANGUAGE OverloadedStrings, RankNTypes #-}
-- | It should be noted that most of the code snippets below depend on the
-- OverloadedStrings language pragma.
--
-- Scotty is set up by default for development mode. For production servers,
-- you will likely want to modify 'Trans.settings' and the 'defaultHandler'. See
-- the comments on each of these functions for more information.
module Web.Scotty
    ( -- * scotty-to-WAI
      scotty, scottyApp, scottyOpts, scottySocket, Options(..)
      -- * Defining Middleware and Routes
      --
      -- | 'Middleware' and routes are run in the order in which they
      -- are defined. All middleware is run first, followed by the first
      -- route that matches. If no route matches, a 404 response is given.
    , middleware, get, post, put, delete, patch, options, addroute, matchAny, notFound
      -- ** Route Patterns
    , capture, regex, function, literal
      -- ** Accessing the Request, Captures, and Query Parameters
    , request, header, headers, body, bodyReader, param, params, jsonData, files
      -- ** Modifying the Response and Redirecting
    , status, addHeader, setHeader, redirect
      -- ** Setting Response Body
      --
      -- | Note: only one of these should be present in any given route
      -- definition, as they completely replace the current 'Response' body.
    , text, html, file, json, stream, raw
      -- ** Exceptions
    , raise, raiseStatus, rescue, next, finish, defaultHandler, liftAndCatchIO
      -- * Parsing Parameters
    , Param, Trans.Parsable(..), Trans.readEither
      -- * Types
    , ScottyM, ActionM, RoutePattern, File
    ) where

-- With the exception of this, everything else better just import types.
import qualified Web.Scotty.Trans as Trans

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Text.Lazy (Text)

import Network.HTTP.Types (Status, StdMethod)
import Network.Socket (Socket)
import Network.Wai (Application, Middleware, Request, StreamingBody)
import Network.Wai.Handler.Warp (Port)

import Web.Scotty.Internal.Types (ScottyT, ActionT, Param, RoutePattern, Options, File)

type ScottyM = ScottyT Text IO
type ActionM = ActionT Text IO

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

-- | Global handler for uncaught exceptions.
--
-- Uncaught exceptions normally become 500 responses.
-- You can use this to selectively override that behavior.
--
-- Note: IO exceptions are lifted into Scotty exceptions by default.
-- This has security implications, so you probably want to provide your
-- own defaultHandler in production which does not send out the error
-- strings as 500 responses.
defaultHandler :: (Text -> ActionM ()) -> ScottyM ()
defaultHandler = Trans.defaultHandler

-- | Use given middleware. Middleware is nested such that the first declared
-- is the outermost middleware (it has first dibs on the request and last action
-- on the response). Every middleware is run on each request.
middleware :: Middleware -> ScottyM ()
middleware = Trans.middleware

-- | Throw an exception, which can be caught with 'rescue'. Uncaught exceptions
-- turn into HTTP 500 responses.
raise :: Text -> ActionM a
raise = Trans.raise

-- | Throw an exception, which can be caught with 'rescue'. Uncaught exceptions turn into HTTP responses corresponding to the given status.
raiseStatus :: Status -> Text -> ActionM a
raiseStatus = Trans.raiseStatus

-- | Abort execution of this action and continue pattern matching routes.
-- Like an exception, any code after 'next' is not executed.
--
-- As an example, these two routes overlap. The only way the second one will
-- ever run is if the first one calls 'next'.
--
-- > get "/foo/:bar" $ do
-- >   w :: Text <- param "bar"
-- >   unless (w == "special") next
-- >   text "You made a request to /foo/special"
-- >
-- > get "/foo/:baz" $ do
-- >   w <- param "baz"
-- >   text $ "You made a request to: " <> w
next :: ActionM a
next = Trans.next

-- | Abort execution of this action. Like an exception, any code after 'finish'
-- is not executed.
--
-- As an example only requests to @\/foo\/special@ will include in the response
-- content the text message.
--
-- > get "/foo/:bar" $ do
-- >   w :: Text <- param "bar"
-- >   unless (w == "special") finish
-- >   text "You made a request to /foo/special"
--
-- /Since: 0.10.3/
finish :: ActionM a
finish = Trans.finish

-- | Catch an exception thrown by 'raise'.
--
-- > raise "just kidding" `rescue` (\msg -> text msg)
rescue :: ActionM a -> (Text -> ActionM a) -> ActionM a
rescue = Trans.rescue

-- | Like 'liftIO', but catch any IO exceptions and turn them into Scotty exceptions.
liftAndCatchIO :: IO a -> ActionM a
liftAndCatchIO = Trans.liftAndCatchIO

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
-- * Raises an exception which can be caught by 'rescue' if parameter is not found.
--
-- * If parameter is found, but 'read' fails to parse to the correct type, 'next' is called.
--   This means captures are somewhat typed, in that a route won't match if a correctly typed
--   capture cannot be parsed.
param :: Trans.Parsable a => Text -> ActionM a
param = Trans.param

-- | Get all parameters from capture, form and query (in that order).
params :: ActionM [Param]
params = Trans.params

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

-- | Define a route with a 'StdMethod', 'Text' value representing the path spec,
-- and a body ('Action') which modifies the response.
--
-- > addroute GET "/" $ text "beam me up!"
--
-- The path spec can include values starting with a colon, which are interpreted
-- as /captures/. These are named wildcards that can be looked up with 'param'.
--
-- > addroute GET "/foo/:bar" $ do
-- >     v <- param "bar"
-- >     text v
--
-- >>> curl http://localhost:3000/foo/something
-- something
addroute :: StdMethod -> RoutePattern -> ActionM () -> ScottyM ()
addroute = Trans.addroute

-- | Match requests using a regular expression.
--   Named captures are not yet supported.
--
-- > get (regex "^/f(.*)r$") $ do
-- >    path <- param "0"
-- >    cap <- param "1"
-- >    text $ mconcat ["Path: ", path, "\nCapture: ", cap]
--
-- >>> curl http://localhost:3000/foo/bar
-- Path: /foo/bar
-- Capture: oo/ba
--
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

-- | Build a route based on a function which can match using the entire 'Request' object.
--   'Nothing' indicates the route does not match. A 'Just' value indicates
--   a successful match, optionally returning a list of key-value pairs accessible
--   by 'param'.
--
-- > get (function $ \req -> Just [("version", pack $ show $ httpVersion req)]) $ do
-- >     v <- param "version"
-- >     text v
--
-- >>> curl http://localhost:3000/
-- HTTP/1.1
--
function :: (Request -> Maybe [Param]) -> RoutePattern
function = Trans.function

-- | Build a route that requires the requested path match exactly, without captures.
literal :: String -> RoutePattern
literal = Trans.literal
