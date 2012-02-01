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
    , middleware, get, post, put, delete, addroute
      -- * Defining Actions
      -- ** Accessing the Request, Captures, and Query Parameters
    , request, param, jsonData
      -- ** Modifying the Response and Redirecting
    , status, header, redirect
      -- ** Setting Response
      --
      -- | Note: only one of these should be present in any given route
      -- definition, as they completely replace the current 'Response' body.
    , text, html, file, json
      -- ** Exceptions
    , raise, rescue, next
      -- * Types
    , ScottyM, ActionM, Parsable
    ) where

import Blaze.ByteString.Builder (fromByteString, fromLazyByteString)

import Control.Applicative
import qualified Control.Exception as E
import qualified Control.DeepSeq as DS
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

import Prelude hiding (catch) -- this always trips me up

import Web.Scotty.Util

data ScottyState = ScottyState {
        middlewares :: [Middleware],
        routes :: [Middleware]
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

type ActionEnv = (Request,[Param],BL.ByteString)

newtype ActionM a = AM { runAM :: ErrorT ActionError (ReaderT ActionEnv (MS.StateT Response IO)) a }
    deriving ( Monad, MonadIO, Functor
             , MonadReader ActionEnv, MS.MonadState Response, MonadError ActionError)

-- Nothing indicates route failed (due to Next) and pattern matching should continue.
-- Just indicates a successful response.
runAction :: ActionEnv -> ActionM () -> ResourceT IO (Maybe Response)
runAction env action = do
    (e,r) <- lift $ flip MS.runStateT def
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
redirect :: T.Text -> ActionM ()
redirect = throwError . Redirect

-- | Get the 'Request' object.
request :: ActionM Request
request = fst3 <$> ask

jsonData :: (A.FromJSON a) => ActionM a
jsonData = do
    body <- thd3 <$> ask
    maybe (raise "unable to parse JSON!") return $ A.decode body

-- | Get a parameter. First looks in captures, then form data, then query parameters.
--
-- * Raises an exception which can be caught by 'rescue' if parameter is not found.
--
-- * If parameter is found, but 'read' fails to parse to the correct type, 'next' is called.
--   This means captures are somewhat typed, in that a route won't match if a correctly typed
--   capture cannot be parsed.
param :: (Parsable a) => T.Text -> ActionM a
param k = do
    val <- lookup k <$> snd3 <$> ask
    case val of
        Nothing -> raise $ mconcat ["Param: ", k, " not found!"]
        Just v  -> maybe next return =<< liftIO (parseParam v)

-- This needs to be in IO to catch parse errors from 'read', but is otherwise pure.
class Parsable a where
    parseParam :: T.Text -> IO (Maybe a)

    -- if any individual element fails to parse, the whole list fails to parse.
    parseParamList :: T.Text -> IO (Maybe [a])
    parseParamList t = sequence <$> mapM parseParam (T.split (==',') t)

-- No point using 'read' for Text, ByteString, Char, and String.
instance Parsable T.Text where parseParam = return . Just
instance Parsable B.ByteString where parseParam = return . Just . lazyTextToStrictByteString
instance Parsable Char where
    parseParam t = case T.unpack t of
                    [c] -> return $ Just c
                    _   -> return Nothing
    parseParamList = return . Just . T.unpack -- String
instance Parsable () where
    parseParam t = if T.null t then return (Just ()) else return Nothing

instance (Parsable a) => Parsable [a] where parseParam = parseParamList

instance Parsable Bool where parseParam = readMaybe
instance Parsable Double where parseParam = readMaybe
instance Parsable Float where parseParam = readMaybe
instance Parsable Int where parseParam = readMaybe
instance Parsable Integer where parseParam = readMaybe

-- Called by parseParam. Uses 'read' to attempt to parse Text value, catching
-- any ErrorCall exceptions (which should be the No Parse error).
readMaybe :: (Read a, DS.NFData a) => T.Text -> IO (Maybe a)
readMaybe tv = E.handleJust E.fromException
                            (\(_::E.ErrorCall) -> return Nothing)
                            $ return DS.$!! Just $ read $ T.unpack tv

-- | get = addroute 'GET'
get :: T.Text -> ActionM () -> ScottyM ()
get = addroute GET

-- | post = addroute 'POST'
post :: T.Text -> ActionM () -> ScottyM ()
post = addroute POST

-- | put = addroute 'PUT'
put :: T.Text -> ActionM () -> ScottyM ()
put = addroute PUT

-- | delete = addroute 'DELETE'
delete :: T.Text -> ActionM () -> ScottyM ()
delete = addroute DELETE

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
addroute :: StdMethod -> T.Text -> ActionM () -> ScottyM ()
addroute method path action = MS.modify (\ (ScottyState ms rs) -> ScottyState ms (r:rs))
    where r = route method withSlash action
          withSlash = case T.uncons path of
                        Just ('/',_) -> path
                        _            -> T.cons '/' path

route :: StdMethod -> T.Text -> ActionM () -> Middleware
route method path action app req =
    if Right method == parseMethod (requestMethod req)
    then case matchRoute path (strictByteStringToLazyText $ rawPathInfo req) of
            Just captures -> do
                body <- BL.fromChunks <$> (lazyConsume $ requestBody req)
                res <- runAction (req, captures ++ formParams method req body ++ queryParams req, body) action
                maybe tryNext return res
            Nothing -> tryNext
    else tryNext
  where tryNext = app req

-- todo: wildcards?
matchRoute :: T.Text -> T.Text -> Maybe [Param]
matchRoute pat req = go (T.split (=='/') pat) (T.split (=='/') req) []
    where go [] [] ps = Just ps -- request string and pattern match!
          go [] r  ps | T.null (mconcat r)  = Just ps -- in case request has trailing slashes
                      | otherwise           = Nothing -- request string is longer than pattern
          go p  [] ps | T.null (mconcat p)  = Just ps -- in case pattern has trailing slashes
                      | otherwise           = Nothing -- request string is not long enough
          go (p:ps) (r:rs) prs | p == r          = go ps rs prs -- equal literals, keeping checking
                               | T.null p        = Nothing      -- p is null, but r is not, fail
                               | T.head p == ':' = go ps rs $ (T.tail p, r) : prs
                                                                -- p is a capture, add to params
                               | otherwise       = Nothing      -- both literals, but unequal, fail

formParams :: StdMethod -> Request -> BL.ByteString -> [Param]
formParams POST req body = case lookup "Content-Type" [(CI.mk k, CI.mk v) | (k,v) <- requestHeaders req] of
                            Just "application/x-www-form-urlencoded" -> parseEncodedParams $ mconcat $ BL.toChunks body
                            Just "application/json" -> []
                            _ -> [] -- do lift $ putStrLn "Unsupported form data encoding. TODO: Fix"
formParams _    _   _ = []

queryParams :: Request -> [Param]
queryParams = parseEncodedParams . rawQueryString

parseEncodedParams :: B.ByteString -> [Param]
parseEncodedParams bs = [ (T.fromStrict k, T.fromStrict $ fromMaybe "" v) | (k,v) <- parseQueryText bs ]

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
