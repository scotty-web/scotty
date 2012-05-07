{-# LANGUAGE OverloadedStrings, FlexibleInstances, ScopedTypeVariables #-}
module Web.Scotty.Route
    ( get, post, put, delete, addroute, matchAny, notFound,
      capture, regex, function, literal, Action
    ) where

import Control.Arrow ((***))
import Control.Applicative
import Control.Monad.Error
import qualified Control.Monad.State as MS
import Control.Monad.Trans.Resource (ResourceT)

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.CaseInsensitive as CI
import Data.Conduit.Lazy (lazyConsume)
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat, (<>))
import qualified Data.Text.Lazy as T
import qualified Data.Text as TS

import Network.HTTP.Types
import Network.Wai

import qualified Text.Regex as Regex

import Web.Scotty.Action
import Web.Scotty.Types

-- | get = 'addroute' 'GET'
get :: (Action action) => RoutePattern -> action -> ScottyM ()
get = addroute GET

-- | post = 'addroute' 'POST'
post :: (Action action) => RoutePattern -> action -> ScottyM ()
post = addroute POST

-- | put = 'addroute' 'PUT'
put :: (Action action) => RoutePattern -> action -> ScottyM ()
put = addroute PUT

-- | delete = 'addroute' 'DELETE'
delete :: (Action action) => RoutePattern -> action -> ScottyM ()
delete = addroute DELETE

-- | Add a route that matches regardless of the HTTP verb.
matchAny :: (Action action) => RoutePattern -> action -> ScottyM ()
matchAny pattern action = mapM_ (\v -> addroute v pattern action) [minBound..maxBound]

-- | Specify an action to take if nothing else is found. Note: this _always_ matches,
-- so should generally be the last route specified.
notFound :: ActionM () -> ScottyM ()
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
addroute :: (Action action) => StdMethod -> RoutePattern -> action -> ScottyM ()
addroute method pat action = MS.modify $ addRoute $ route method pat $ build action pat

-- | An action (executed when a route matches) can either be an 'ActionM' computation, or
-- a function with an argument for each capture in the route. For example:
--
-- > get "/lambda/:foo/:bar" $ \ a b -> do
-- >     text $ mconcat [a,b]
--
-- is elaborated by Scotty to:
--
-- > get "/lambda/:foo/:bar" $ do
-- >     a <- param "foo"
-- >     b <- param "bar"
-- >     text $ mconcat [a,b]
class Action a where
    build :: a -> RoutePattern -> ActionM ()

instance Action (ActionM a) where
    build action _ = action >> return ()

instance (Parsable a, Action b) => Action (a -> b) where
    build f pat = findCapture pat >>= \ (v, pat') -> build (f v) pat'
        where findCapture :: RoutePattern -> ActionM (a, RoutePattern)
              findCapture (Literal l) = raise $ "Lambda trying to capture a literal route: " <> l
              findCapture (Capture p) = case T.span (/='/') (T.dropWhile (/=':') p) of
                                            (m,r) | T.null m -> raise "More function arguments than captures."
                                                  | otherwise -> param (T.tail m) >>= \ v -> return (v, Capture r)
              findCapture (Function _) = raise "Lambda trying to capture a function route."

route :: StdMethod -> RoutePattern -> ActionM () -> Middleware
route method pat action app req =
    if Right method == parseMethod (requestMethod req)
    then case matchRoute pat req of
            Just captures -> do
                env <- mkEnv method req captures
                res <- lift $ runAction env action
                maybe tryNext return res
            Nothing -> tryNext
    else tryNext
  where tryNext = app req

matchRoute :: RoutePattern -> Request -> Maybe [Param]

matchRoute (Literal pat) req | pat == path req = Just []
                             | otherwise       = Nothing

matchRoute (Function fun) req = fun req

matchRoute (Capture pat) req = go (T.split (=='/') pat) (T.split (=='/') $ path req) []
    where go [] [] prs = Just prs -- request string and pattern match!
          go [] r  prs | T.null (mconcat r)  = Just prs -- in case request has trailing slashes
                       | otherwise           = Nothing  -- request string is longer than pattern
          go p  [] prs | T.null (mconcat p)  = Just prs -- in case pattern has trailing slashes
                       | otherwise           = Nothing  -- request string is not long enough
          go (p:ps) (r:rs) prs | p == r          = go ps rs prs -- equal literals, keeping checking
                               | T.null p        = Nothing      -- p is null, but r is not, fail
                               | T.head p == ':' = go ps rs $ (T.tail p, r) : prs -- p is a capture, add to params
                               | otherwise       = Nothing      -- both literals, but unequal, fail

-- Pretend we are at the top level.
path :: Request -> T.Text
path = T.fromStrict . TS.cons '/' . TS.intercalate "/" . pathInfo

mkEnv :: StdMethod -> Request -> [Param] -> ResourceT IO ActionEnv
mkEnv method req captures = do
    b <- BL.fromChunks <$> lazyConsume (requestBody req)

    let parameters = captures ++ formparams ++ queryparams
        formparams = case (method, lookup "Content-Type" [(CI.mk k, CI.mk v) | (k,v) <- requestHeaders req]) of
                        (_, Just "application/x-www-form-urlencoded") -> parseEncodedParams $ mconcat $ BL.toChunks b
                        _ -> []
        queryparams = parseEncodedParams $ rawQueryString req

    return $ Env req parameters b

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
capture = Capture . T.pack

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
