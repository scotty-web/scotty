{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
-- | It should be noted that most of the code snippets below depend on the
-- OverloadedStrings language pragma.
module Web.Scotty
    ( -- * scotty-to-WAI
      scotty, scottyApp, scottyOpts, scottySSL, Options(..)
      -- * Defining Middleware and Routes
      --
      -- | 'Middleware' and routes are run in the order in which they
      -- are defined. All middleware is run first, followed by the first
      -- route that matches. If no route matches, a 404 response is given.
    , middleware, get, post, put, delete, patch, addroute, matchAny, notFound
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
    , text, html, file, json, source, raw
      -- ** Exceptions
    , raise, rescue, next
      -- * Parsing Parameters
    , Param, Trans.Parsable(..), Trans.readEither
      -- * Types
    , ScottyM, ActionM, RoutePattern, File
    ) where

-- With the exception of this, everything else better just import types.
import qualified Web.Scotty.Trans           as Trans

import           Blaze.ByteString.Builder   (Builder)

import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Conduit               (Flush, ResourceT, Source)
import           Data.Text.Lazy             (Text)

import           Network.HTTP.Types         (Status, StdMethod)
import           Network.Wai                (Application, Middleware, Request)
import           Network.Wai.Handler.Warp   (Port)

import           Web.Scotty.Types           (ActionM, File, Options, Param,
                                             RoutePattern, ScottyM)

-- | Run a scotty application using the warp server.
scotty :: Port -> ScottyM () -> IO ()
scotty p = Trans.scottyT p id id

-- | Run a scotty application using the warp server over ssl.
scottySSL :: Port -> FilePath -> FilePath -> ScottyM () -> IO ()
scottySSL p cert key = Trans.scottySslT cert key p id id

-- | Run a scotty application using the warp server, passing extra options.
scottyOpts :: Options -> ScottyM () -> IO ()
scottyOpts opts = Trans.scottyOptsT opts id id

-- | Turn a scotty application into a WAI 'Application', which can be
-- run with any WAI handler.
scottyApp :: ScottyM () -> IO Application
scottyApp = Trans.scottyAppT id id

-- | Use given middleware. Middleware is nested such that the first declared
-- is the outermost middleware (it has first dibs on the request and last action
-- on the response). Every middleware is run on each request.
middleware :: Middleware -> ScottyM ()
middleware = Trans.middleware

-- | Throw an exception, which can be caught with 'rescue'. Uncaught exceptions
-- turn into HTTP 500 responses.
raise :: Text -> ActionM a
raise = Trans.raise

-- | Abort execution of this action and continue pattern matching routes.
-- Like an exception, any code after 'next' is not executed.
--
-- As an example, these two routes overlap. The only way the second one will
-- ever run is if the first one calls 'next'.
--
-- > get "/foo/:number" $ do
-- >   n <- param "number"
-- >   unless (all isDigit n) $ next
-- >   text "a number"
-- >
-- > get "/foo/:bar" $ do
-- >   bar <- param "bar"
-- >   text "not a number"
next :: ActionM a
next = Trans.next

-- | Catch an exception thrown by 'raise'.
--
-- > raise "just kidding" `rescue` (\msg -> text msg)
rescue :: ActionM a -> (Text -> ActionM a) -> ActionM a
rescue = Trans.rescue

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
reqHeader :: Text -> ActionM (Maybe Text)
reqHeader = Trans.reqHeader

-- | Get the request body.
body :: ActionM ByteString
body = Trans.body

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

-- | Set one of the response headers. Will override any previously set value for that header.
-- Header names are case-insensitive.
header :: Text -> Text -> ActionM ()
header = Trans.header

-- | Set the body of the response to the given 'Text' value. Also sets \"Content-Type\"
-- header to \"text/plain\".
text :: Text -> ActionM ()
text = Trans.text

-- | Set the body of the response to the given 'Text' value. Also sets \"Content-Type\"
-- header to \"text/html\".
html :: Text -> ActionM ()
html = Trans.html

-- | Send a file as the response. Doesn't set the \"Content-Type\" header, so you probably
-- want to do that on your own with 'header'.
file :: FilePath -> ActionM ()
file = Trans.file

-- | Set the body of the response to the JSON encoding of the given value. Also sets \"Content-Type\"
-- header to \"application/json\".
json :: ToJSON a => a -> ActionM ()
json = Trans.json

-- | Set the body of the response to a Source. Doesn't set the
-- \"Content-Type\" header, so you probably want to do that on your
-- own with 'header'.
source :: Source (ResourceT IO) (Flush Builder) -> ActionM ()
source = Trans.source

-- | Set the body of the response to the given 'BL.ByteString' value. Doesn't set the
-- \"Content-Type\" header, so you probably want to do that on your own with 'header'.
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
