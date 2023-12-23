{-# LANGUAGE FlexibleContexts, FlexibleInstances,
             OverloadedStrings, RankNTypes, ScopedTypeVariables #-}
module Web.Scotty.Route
    ( get, post, put, delete, patch, options, addroute, matchAny, notFound,
      capture, regex, function, literal
    ) where

import           Control.Arrow ((***))
import Control.Concurrent.STM (newTVarIO)
import           Control.Monad.IO.Class (MonadIO(..))
import UnliftIO (MonadUnliftIO(..))
import qualified Control.Monad.State as MS

import           Data.String (fromString)
import qualified Data.Text as T

import           Network.HTTP.Types
import           Network.Wai (Request(..))

import qualified Text.Regex as Regex

import           Web.Scotty.Action
import           Web.Scotty.Internal.Types (RoutePattern(..), RouteOptions, ActionEnv(..), ActionT, ScottyState(..), ScottyT(..), ErrorHandler, Middleware, BodyInfo, handler, addRoute, defaultScottyResponse)
import           Web.Scotty.Util (decodeUtf8Lenient)
import Web.Scotty.Body (cloneBodyInfo, getBodyAction, getBodyChunkAction, getFormParamsAndFilesAction)

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

-- | get = 'addroute' 'GET'
get :: (MonadUnliftIO m) => RoutePattern -> ActionT m () -> ScottyT m ()
get = addroute GET

-- | post = 'addroute' 'POST'
post :: (MonadUnliftIO m) => RoutePattern -> ActionT m () -> ScottyT m ()
post = addroute POST

-- | put = 'addroute' 'PUT'
put :: (MonadUnliftIO m) => RoutePattern -> ActionT m () -> ScottyT m ()
put = addroute PUT

-- | delete = 'addroute' 'DELETE'
delete :: (MonadUnliftIO m) => RoutePattern -> ActionT m () -> ScottyT m ()
delete = addroute DELETE

-- | patch = 'addroute' 'PATCH'
patch :: (MonadUnliftIO m) => RoutePattern -> ActionT m () -> ScottyT m ()
patch = addroute PATCH

-- | options = 'addroute' 'OPTIONS'
options :: (MonadUnliftIO m) => RoutePattern -> ActionT m () -> ScottyT m ()
options = addroute OPTIONS

-- | Add a route that matches regardless of the HTTP verb.
matchAny :: (MonadUnliftIO m) => RoutePattern -> ActionT m () -> ScottyT m ()
matchAny pat action = ScottyT $ MS.modify $ \s -> addRoute (route (routeOptions s) (handler s) Nothing pat action) s

-- | Specify an action to take if nothing else is found. Note: this _always_ matches,
-- so should generally be the last route specified.
notFound :: (MonadUnliftIO m) => ActionT m () -> ScottyT m ()
notFound action = matchAny (Function (\req -> Just [("path", path req)])) (status status404 >> action)

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
addroute :: (MonadUnliftIO m) => StdMethod -> RoutePattern -> ActionT m () -> ScottyT m ()
addroute method pat action = ScottyT $ MS.modify $ \s -> addRoute (route (routeOptions s) (handler s) (Just method) pat action) s

route :: (MonadUnliftIO m) =>
         RouteOptions
      -> Maybe (ErrorHandler m) -> Maybe StdMethod -> RoutePattern -> ActionT m () -> BodyInfo -> Middleware m
route opts h method pat action bodyInfo app req =
  let tryNext = app req
      -- We match all methods in the case where 'method' is 'Nothing'.
      -- See https://github.com/scotty-web/scotty/issues/196 and 'matchAny'
      methodMatches :: Bool
      methodMatches = maybe True (\x -> (Right x == parseMethod (requestMethod req))) method

  in if methodMatches
     then case matchRoute pat req of
            Just captures -> do
              -- The user-facing API for "body" and "bodyReader" involve an IO action that
              -- reads the body/chunks thereof only once, so we shouldn't pass in our BodyInfo
              -- directly; otherwise, the body might get consumed and then it would be unavailable
              -- if `next` is called and we try to match further routes.
              -- Instead, make a "cloned" copy of the BodyInfo that allows the IO actions to be called
              -- without messing up the state of the original BodyInfo.
              clonedBodyInfo <- cloneBodyInfo bodyInfo

              env <- mkEnv clonedBodyInfo req captures opts
              res <- runAction h env action
              maybe tryNext return res
            Nothing -> tryNext
     else tryNext

matchRoute :: RoutePattern -> Request -> Maybe [Param]
matchRoute (Literal pat)  req | pat == path req = Just []
                              | otherwise       = Nothing
matchRoute (Function fun) req = fun req
matchRoute (Capture pat)  req = go (T.split (=='/') pat) (compress $ "":pathInfo req) [] -- add empty segment to simulate being at the root
    where go [] [] prs = Just prs -- request string and pattern match!
          go [] r  prs | T.null (mconcat r)  = Just prs -- in case request has trailing slashes
                       | otherwise           = Nothing  -- request string is longer than pattern
          go p  [] prs | T.null (mconcat p)  = Just prs -- in case pattern has trailing slashes
                       | otherwise           = Nothing  -- request string is not long enough
          go (p:ps) (r:rs) prs = case T.uncons p of
                        Just (':', name) -> go ps rs $ (name, r) : prs -- p is a capture, add to params
                        _ | p == r       -> go ps rs prs -- equal literals, keeping checking
                          | otherwise    -> Nothing      -- both literals, but unequal, fail
          compress ("":rest@("":_)) = compress rest
          compress (x:xs) = x : compress xs
          compress [] = []

-- Pretend we are at the top level.
path :: Request -> T.Text
path = T.cons '/' . T.intercalate "/" . pathInfo

-- | Parse the request and construct the initial 'ActionEnv' with a default 200 OK response
mkEnv :: MonadIO m => BodyInfo -> Request -> [Param] -> RouteOptions -> m ActionEnv
mkEnv bodyInfo req captureps opts = do
  (formps, bodyFiles) <- liftIO $ getFormParamsAndFilesAction req bodyInfo opts
  let
    queryps = parseEncodedParams $ queryString req
    bodyFiles' = [ (decodeUtf8Lenient k, fi) | (k,fi) <- bodyFiles ]
  responseInit <- liftIO $ newTVarIO defaultScottyResponse
  return $ Env req captureps formps queryps (getBodyAction bodyInfo opts) (getBodyChunkAction bodyInfo) bodyFiles' responseInit


parseEncodedParams :: Query -> [Param]
parseEncodedParams qs = [ ( decodeUtf8Lenient k, maybe "" decodeUtf8Lenient v) | (k,v) <- qs ]

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
regex pat = Function $ \ req -> fmap (map (T.pack . show *** T.pack) . zip [0 :: Int ..] . strip)
                                         (Regex.matchRegexAll rgx $ T.unpack $ path req)
    where rgx = Regex.mkRegex pat
          strip (_, match, _, subs) = match : subs

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
capture = fromString

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
function = Function

-- | Build a route that requires the requested path match exactly, without captures.
literal :: String -> RoutePattern
literal = Literal . T.pack
