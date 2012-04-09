{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
-- | It should be noted that most of the code snippets below depend on the
-- OverloadedStrings language pragma.
module Web.Scotty
    ( -- * scotty-to-WAI
      scotty, scottyApp
      -- * Defining Middleware and Routes
      --
      -- | 'Middleware' and routes are run in the order in which they
      -- are defined. All middleware is run first, followed by the first
      -- route that matches. If no route matches, a 404 response is given.
    , middleware, get, post, put, delete, addroute, matchAll
      -- * Defining Actions
      -- ** Accessing the Request, Captures, and Query Parameters
    , request, body, param, params, jsonData
      -- ** Modifying the Response and Redirecting
    , status, header, redirect
      -- ** Setting Response Body
      --
      -- | Note: only one of these should be present in any given route
      -- definition, as they completely replace the current 'Response' body.
    , text, html, file, json
      -- ** Exceptions
    , raise, rescue, next
      -- * Types
    , ScottyM, ActionM, Parsable
    , RoutePattern(..)
    ) where

import Blaze.ByteString.Builder (fromByteString, fromLazyByteString)

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import qualified Control.Monad.State as MS
import Control.Monad.Trans.Resource (ResourceT)

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.CaseInsensitive as CI
import Data.Default (Default, def)
import Data.Conduit.Lazy (lazyConsume)
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (encodeUtf8)

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (Port, run)

import Web.Scotty.Util
import Data.String

import qualified Text.Regex as Regex
import Control.Arrow ((***))

-- | Provides an interface for defining how different routes can be specified
--   This includes three options:
--
--   Keyword  - The standard approach to Sinatra style routes
--              GET "/users/sam"   -> Keyword "/users/:user" -> Just [("user","sam")]
--
--   Function - Let the user specify how their route matches
--              GET "/users/sam"   -> Function (const (Just [("hello", "world")])) -> Just [("hello","world")]
--
--   Literal  - Ignore route parameters and match literally
--              GET "/users/sam"   -> Literal "/users/:user" -> Nothing
--              GET "/users/:user" -> Literal "/users/:user" -> Just []
--
--   Regex    - Match path against a regular expression.
--              GET "/users/sam" -> regexRoute "^/u(.*)m$" -> Just [("0", "/users/sam"), ("1","sers/sa")]
--
data RoutePattern = Keyword   T.Text
                  | Literal   T.Text
                  | Regex     String
                  | Function (T.Text -> Maybe [Param])

-- Provides a shorthand for creating a regex-based route pattern
-- No named captures are supported at this point and instead you
-- look up each match via its (Text) regex index number.
--
--   GET "/users/sam" -> regexRoute "^/u(.*)m$" -> Just [("0", "/users/sam"), ("1","sers/sa")]
--
regexRoute :: String -> RoutePattern
regexRoute pattern = Function rr
  where
    rr t = results
      where
        txt     = T.unpack t
        regex   = Regex.mkRegex pattern
        results = fmap (map (T.pack . show *** T.pack) . zip [0 :: Int ..] . strip)
                       (Regex.matchRegexAll regex txt)
        strip (_, match, _, subs) = match : subs

instance IsString RoutePattern where fromString x = Keyword (T.pack x)

data ScottyState = ScottyState { middlewares :: [Middleware]
                               , routes :: [Middleware]
                               }

instance Default ScottyState where
    def = ScottyState [] []

newtype ScottyM a = S { runS :: MS.StateT ScottyState IO a }
    deriving (Monad, MonadIO, Functor, MS.MonadState ScottyState)

-- | Run a scotty application using the warp server.
scotty :: Port -> ScottyM () -> IO ()
scotty p s = putStrLn "Setting phasers to stun... (ctrl-c to quit)" >> (run p =<< scottyApp s)

-- | Turn a scotty application into a WAI 'Application', which can be
-- run with any WAI handler.
scottyApp :: ScottyM () -> IO Application
scottyApp defs = do
    s <- MS.execStateT (runS defs) def
    return $ foldl (flip ($)) notFoundApp $ routes s ++ middlewares s

notFoundApp :: Application
notFoundApp _ = return $ ResponseBuilder status404 [("Content-Type","text/html")]
                       $ fromByteString "<h1>404: File Not Found!</h1>"

-- | Use given middleware. Middleware is nested such that the first declared
-- is the outermost middleware (it has first dibs on the request and last action
-- on the response). Every middleware is run on each request.
middleware :: Middleware -> ScottyM ()
middleware m = MS.modify (\ (ScottyState ms rs) -> ScottyState (m:ms) rs)

type Param = (T.Text, T.Text)

data ActionError = Redirect T.Text
                 | ActionError T.Text
                 | Next
    deriving (Eq,Show)

instance Error ActionError where
    strMsg = ActionError . T.pack

data ActionEnv = Env { getReq :: Request, getParams :: [Param], getBody :: BL.ByteString }

newtype ActionM a = AM { runAM :: ErrorT ActionError (ReaderT ActionEnv (MS.StateT Response IO)) a }
    deriving ( Monad, MonadIO, Functor
             , MonadReader ActionEnv, MS.MonadState Response, MonadError ActionError)

-- Nothing indicates route failed (due to Next) and pattern matching should continue.
-- Just indicates a successful response.
runAction :: ActionEnv -> ActionM () -> IO (Maybe Response)
runAction env action = do
    (e,r) <- flip MS.runStateT def
           $ flip runReaderT env
           $ runErrorT
           $ runAM
           $ action `catchError` defaultHandler
    return $ either (const Nothing) (const $ Just r) e

defaultHandler :: ActionError -> ActionM ()
defaultHandler (Redirect url) = do
    status status302
    header "Location" url
defaultHandler (ActionError msg) = do
    status status500
    html $ mconcat ["<h1>500 Internal Server Error</h1>", msg]
defaultHandler Next = next

-- | Throw an exception, which can be caught with 'rescue'. Uncaught exceptions
-- turn into HTTP 500 responses.
raise :: T.Text -> ActionM a
raise = throwError . ActionError

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
next = throwError Next

-- | Catch an exception thrown by 'raise'.
--
-- > raise "just kidding" `rescue` (\msg -> text msg)
rescue :: ActionM a -> (T.Text -> ActionM a) -> ActionM a
rescue action handler = catchError action $ \e -> case e of
    ActionError msg -> handler msg      -- handle errors
    other           -> throwError other -- rethrow redirects and nexts

-- | Redirect to given URL. Like throwing an uncatchable exception. Any code after the call to redirect
-- will not be run.
--
-- > redirect "http://www.google.com"
--
-- OR
--
-- > redirect "/foo/bar"
redirect :: T.Text -> ActionM a
redirect = throwError . Redirect

-- | Get the 'Request' object.
request :: ActionM Request
request = getReq <$> ask

-- | Get the request body.
body :: ActionM BL.ByteString
body = getBody <$> ask

-- | Parse the request body as a JSON object and return it. Raises an exception if parse is unsuccessful.
jsonData :: (A.FromJSON a) => ActionM a
jsonData = do
    b <- body
    maybe (raise "jsonData: no parse") return $ A.decode b

-- | Get a parameter. First looks in captures, then form data, then query parameters.
--
-- * Raises an exception which can be caught by 'rescue' if parameter is not found.
--
-- * If parameter is found, but 'read' fails to parse to the correct type, 'next' is called.
--   This means captures are somewhat typed, in that a route won't match if a correctly typed
--   capture cannot be parsed.
param :: (Parsable a) => T.Text -> ActionM a
param k = do
    val <- lookup k <$> getParams <$> ask
    case val of
        Nothing -> raise $ mconcat ["Param: ", k, " not found!"]
        Just v  -> either (const next) return $ parseParam v

params :: ActionM [(T.Text, T.Text)]
params = getParams <$> ask

class Parsable a where
    parseParam :: T.Text -> Either T.Text a

    -- if any individual element fails to parse, the whole list fails to parse.
    parseParamList :: T.Text -> Either T.Text [a]
    parseParamList t = sequence $ map parseParam (T.split (==',') t)

-- No point using 'read' for Text, ByteString, Char, and String.
instance Parsable T.Text where parseParam = Right
instance Parsable B.ByteString where parseParam = Right . lazyTextToStrictByteString
instance Parsable Char where
    parseParam t = case T.unpack t of
                    [c] -> Right c
                    _   -> Left "parseParam Char: no parse"
    parseParamList = Right . T.unpack -- String
instance Parsable () where
    parseParam t = if T.null t then Right () else Left "parseParam Unit: no parse"

instance (Parsable a) => Parsable [a] where parseParam = parseParamList

instance Parsable Bool where parseParam = readEither
instance Parsable Double where parseParam = readEither
instance Parsable Float where parseParam = readEither
instance Parsable Int where parseParam = readEither
instance Parsable Integer where parseParam = readEither

readEither :: (Read a) => T.Text -> Either T.Text a
readEither t = case [ x | (x,"") <- reads (T.unpack t) ] of
                [x] -> Right x
                []  -> Left "readEither: no parse"
                _   -> Left "readEither: ambiguous parse"

-- | get = addroute 'GET'
get :: RoutePattern -> ActionM () -> ScottyM ()
get = addroute GET

-- | post = addroute 'POST'
post :: RoutePattern -> ActionM () -> ScottyM ()
post = addroute POST

-- | put = addroute 'PUT'
put :: RoutePattern -> ActionM () -> ScottyM ()
put = addroute PUT

-- | delete = addroute 'DELETE'
delete :: RoutePattern -> ActionM () -> ScottyM ()
delete = addroute DELETE

-- | matchAll = Add a route of each type
matchAll action = mapM_ (match action) [get, post, put, delete]
  where
    match action method = method matchall $ action
    matchall = Function (\x -> Just [("path", x)])

-- | Define a route with a 'StdMethod', 'T.Text' value representing the path spec,
-- and a body ('ActionM') which modifies the response.
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
addroute method path action = MS.modify (\ (ScottyState ms rs) -> ScottyState ms (r:rs))
    where r = route method path action

route :: StdMethod -> RoutePattern -> ActionM () -> Middleware
route method path action app req =
    if Right method == parseMethod (requestMethod req)
    then case matchRoute path (strictByteStringToLazyText $ rawPathInfo req) of
            Just captures -> do
                env <- mkEnv method req captures
                res <- lift $ runAction env action
                maybe tryNext return res
            Nothing -> tryNext
    else tryNext
  where tryNext = app req

mkEnv :: StdMethod -> Request -> [Param] -> ResourceT IO ActionEnv
mkEnv method req captures = do
    b <- BL.fromChunks <$> (lazyConsume $ requestBody req)

    let params = captures ++ formparams ++ queryparams
        formparams = case (method, lookup "Content-Type" [(CI.mk k, CI.mk v) | (k,v) <- requestHeaders req]) of
                        (POST, Just "application/x-www-form-urlencoded") -> parseEncodedParams $ mconcat $ BL.toChunks b
                        _ -> []
        queryparams = parseEncodedParams $ rawQueryString req

    return $ Env req params b

parseEncodedParams :: B.ByteString -> [Param]
parseEncodedParams bs = [ (T.fromStrict k, T.fromStrict $ fromMaybe "" v) | (k,v) <- parseQueryText bs ]

-- todo: wildcards?
matchRoute :: RoutePattern -> T.Text -> Maybe [Param]

matchRoute (Literal pat) req | pat == req = Just []
                             | otherwise  = Nothing

matchRoute (Regex pat) req = matchRoute (regexRoute pat) req

matchRoute (Function fun) req = fun req

matchRoute (Keyword pat) req = go (T.split (=='/') pat) (T.split (=='/') req) []
    where go [] [] prs = Just prs -- request string and pattern match!
          go [] r  prs | T.null (mconcat r)  = Just prs -- in case request has trailing slashes
                       | otherwise           = Nothing  -- request string is longer than pattern
          go p  [] prs | T.null (mconcat p)  = Just prs -- in case pattern has trailing slashes
                       | otherwise           = Nothing  -- request string is not long enough
          go (p:ps) (r:rs) prs | p == r          = go ps rs prs -- equal literals, keeping checking
                               | T.null p        = Nothing      -- p is null, but r is not, fail
                               | T.head p == ':' = go ps rs $ (T.tail p, r) : prs -- p is a capture, add to params
                               | otherwise       = Nothing      -- both literals, but unequal, fail

-- | Set the HTTP response status. Default is 200.
status :: Status -> ActionM ()
status = MS.modify . setStatus

-- | Set one of the response headers. Will override any previously set value for that header.
-- Header names are case-insensitive.
header :: T.Text -> T.Text -> ActionM ()
header k v = MS.modify $ setHeader (CI.mk $ lazyTextToStrictByteString k, lazyTextToStrictByteString v)

-- | Set the body of the response to the given 'T.Text' value. Also sets \"Content-Type\"
-- header to \"text/plain\".
text :: T.Text -> ActionM ()
text t = do
    header "Content-Type" "text/plain"
    MS.modify $ setContent $ Left $ fromLazyByteString $ encodeUtf8 t

-- | Set the body of the response to the given 'T.Text' value. Also sets \"Content-Type\"
-- header to \"text/html\".
html :: T.Text -> ActionM ()
html t = do
    header "Content-Type" "text/html"
    MS.modify $ setContent $ Left $ fromLazyByteString $ encodeUtf8 t

-- | Send a file as the response. Doesn't set the \"Content-Type\" header, so you probably
-- want to do that on your own with 'header'.
file :: FilePath -> ActionM ()
file = MS.modify . setContent . Right

-- | Set the body of the response to the JSON encoding of the given value. Also sets \"Content-Type\"
-- header to \"application/json\".
json :: (A.ToJSON a) => a -> ActionM ()
json v = do
    header "Content-Type" "application/json"
    MS.modify $ setContent $ Left $ fromLazyByteString $ A.encode v
