{-# LANGUAGE FlexibleContexts, FlexibleInstances,
             OverloadedStrings, RankNTypes, ScopedTypeVariables #-}
module Web.Scotty.Route
    ( get, post, put, delete, patch, options, addroute, matchAny, notFound,
      capture, regex, function, literal
    ) where

import           Control.Arrow ((***))
import           Control.Monad.IO.Class
import qualified Control.Monad.State as MS

import qualified Data.ByteString.Char8 as B

import           Data.Maybe (fromMaybe)
import           Data.String (fromString)
import qualified Data.Text.Lazy as T
import qualified Data.Text as TS

import           Network.HTTP.Types
import           Network.Wai (Request(..))

import           Prelude ()
import           Prelude.Compat

import qualified Text.Regex as Regex

import           Web.Scotty.Action
import           Web.Scotty.Internal.Types (RoutePattern(..), RouteOptions, ActionEnv(..), ActionT, ScottyState(..), ScottyT(..), Middleware, BodyInfo, ScottyError(..), ErrorHandler, handler, addRoute)
import           Web.Scotty.Util (strictByteStringToLazyText)
import Web.Scotty.Body (cloneBodyInfo, getBodyAction, getBodyChunkAction, getFormParamsAndFilesAction)

-- | get = 'addroute' 'GET'
get :: (ScottyError e, MonadIO m) => RoutePattern -> ActionT e m () -> ScottyT e m ()
get = addroute GET

-- | post = 'addroute' 'POST'
post :: (ScottyError e, MonadIO m) => RoutePattern -> ActionT e m () -> ScottyT e m ()
post = addroute POST

-- | put = 'addroute' 'PUT'
put :: (ScottyError e, MonadIO m) => RoutePattern -> ActionT e m () -> ScottyT e m ()
put = addroute PUT

-- | delete = 'addroute' 'DELETE'
delete :: (ScottyError e, MonadIO m) => RoutePattern -> ActionT e m () -> ScottyT e m ()
delete = addroute DELETE

-- | patch = 'addroute' 'PATCH'
patch :: (ScottyError e, MonadIO m) => RoutePattern -> ActionT e m () -> ScottyT e m ()
patch = addroute PATCH

-- | options = 'addroute' 'OPTIONS'
options :: (ScottyError e, MonadIO m) => RoutePattern -> ActionT e m () -> ScottyT e m ()
options = addroute OPTIONS

-- | Add a route that matches regardless of the HTTP verb.
matchAny :: (ScottyError e, MonadIO m) => RoutePattern -> ActionT e m () -> ScottyT e m ()
matchAny pattern action = ScottyT $ MS.modify $ \s -> addRoute (route (routeOptions s) (handler s) Nothing pattern action) s

-- | Specify an action to take if nothing else is found. Note: this _always_ matches,
-- so should generally be the last route specified.
notFound :: (ScottyError e, MonadIO m) => ActionT e m () -> ScottyT e m ()
notFound action = matchAny (Function (\req -> Just [("path", path req)])) (status status404 >> action)

-- | Define a route with a 'StdMethod', 'T.Text' value representing the path spec,
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
addroute :: (ScottyError e, MonadIO m) => StdMethod -> RoutePattern -> ActionT e m () -> ScottyT e m ()
addroute method pat action = ScottyT $ MS.modify $ \s -> addRoute (route (routeOptions s) (handler s) (Just method) pat action) s

route :: (ScottyError e, MonadIO m) =>
         RouteOptions -> ErrorHandler e m -> Maybe StdMethod -> RoutePattern -> ActionT e m () -> BodyInfo -> Middleware m
route opts h method pat action bodyInfo app req =
  let tryNext = app req
        {- |
          We match all methods in the case where 'method' is 'Nothing'.
          See https://github.com/scotty-web/scotty/issues/196
        -}
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
matchRoute (Capture pat)  req = go (T.split (=='/') pat) (compress $ T.split (=='/') $ path req) []
    where go [] [] prs = Just prs -- request string and pattern match!
          go [] r  prs | T.null (mconcat r)  = Just prs -- in case request has trailing slashes
                       | otherwise           = Nothing  -- request string is longer than pattern
          go p  [] prs | T.null (mconcat p)  = Just prs -- in case pattern has trailing slashes
                       | otherwise           = Nothing  -- request string is not long enough
          go (p:ps) (r:rs) prs | p == r          = go ps rs prs -- equal literals, keeping checking
                               | T.null p        = Nothing      -- p is null, but r is not, fail
                               | T.head p == ':' = go ps rs $ (T.tail p, r) : prs -- p is a capture, add to params
                               | otherwise       = Nothing      -- both literals, but unequal, fail
          compress ("":rest@("":_)) = compress rest
          compress (x:xs) = x : compress xs
          compress [] = []

-- Pretend we are at the top level.
path :: Request -> T.Text
path = T.fromStrict . TS.cons '/' . TS.intercalate "/" . pathInfo


mkEnv :: MonadIO m => BodyInfo -> Request -> [Param] -> RouteOptions -> m ActionEnv
mkEnv bodyInfo req captureps opts = do
  (formps, bodyFiles) <- liftIO $ getFormParamsAndFilesAction req bodyInfo opts
  let
    queryps = parseEncodedParams $ rawQueryString req
    bodyFiles' = [ (strictByteStringToLazyText k, fi) | (k,fi) <- bodyFiles ]
  return $ Env req captureps formps queryps (getBodyAction bodyInfo opts) (getBodyChunkAction bodyInfo) bodyFiles'


parseEncodedParams :: B.ByteString -> [Param]
parseEncodedParams bs = [ (T.fromStrict k, T.fromStrict $ fromMaybe "" v) | (k,v) <- parseQueryText bs ]

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
regex pattern = Function $ \ req -> fmap (map (T.pack . show *** T.pack) . zip [0 :: Int ..] . strip)
                                         (Regex.matchRegexAll rgx $ T.unpack $ path req)
    where rgx = Regex.mkRegex pattern
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

-- | Build a route based on a function which can match using the entire 'Request' object.
--   'Nothing' indicates the route does not match. A 'Just' value indicates
--   a successful match, optionally returning a list of key-value pairs accessible
--   by 'param'.
--
-- > get (function $ \req -> Just [("version", T.pack $ show $ httpVersion req)]) $ do
-- >     v <- param "version"
-- >     text v
--
-- >>> curl http://localhost:3000/
-- HTTP/1.1
--
function :: (Request -> Maybe [Param]) -> RoutePattern
function = Function

-- | Build a route that requires the requested path match exactly, without captures.
literal :: String -> RoutePattern
literal = Literal . T.pack
