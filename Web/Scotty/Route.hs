{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances,
             OverloadedStrings, RankNTypes, ScopedTypeVariables #-}
module Web.Scotty.Route
    ( get, post, put, delete, patch, addroute, matchAny, notFound,
      capture, regex, function, literal
    ) where

import           Control.Arrow ((***))
import           Control.Concurrent.MVar
#if MIN_VERSION_mtl(2,2,1)
import           Control.Monad.Except
#else
import           Control.Monad.Error
#endif
import qualified Control.Monad.State as MS

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Maybe (fromMaybe)
import           Data.Monoid (mconcat)
import           Data.String (fromString)
import qualified Data.Text.Lazy as T
import qualified Data.Text as TS

import           Network.HTTP.Types
import           Network.Wai (Request(..))
import qualified Network.Wai.Parse as Parse hiding (parseRequestBody)

import qualified Text.Regex as Regex

import           Web.Scotty.Action
import           Web.Scotty.Internal.Types
import           Web.Scotty.Util

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

-- | Add a route that matches regardless of the HTTP verb.
matchAny :: (ScottyError e, MonadIO m) => RoutePattern -> ActionT e m () -> ScottyT e m ()
matchAny pattern action = mapM_ (\v -> addroute v pattern action) [minBound..maxBound]

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
addroute method pat action = ScottyT $ MS.modify $ \s -> addRoute (route (handler s) method pat action) s

route :: (ScottyError e, MonadIO m) => ErrorHandler e m -> StdMethod -> RoutePattern -> ActionT e m () -> Middleware m
route h method pat action app req =
    let tryNext = app req
    in if Right method == parseMethod (requestMethod req)
       then case matchRoute pat req of
            Just captures -> do
                env <- mkEnv req captures
                res <- runAction h env action
                maybe tryNext return res
            Nothing -> tryNext
       else tryNext

matchRoute :: RoutePattern -> Request -> Maybe [Param]
matchRoute (Literal pat)  req | pat == path req = Just []
                              | otherwise       = Nothing
matchRoute (Function fun) req = fun req
matchRoute (Capture pat)  req = go (T.split (=='/') pat) (T.split (=='/') $ path req) []
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

-- Stolen from wai-extra's Network.Wai.Parse, modified to accept body as list of Bytestrings.
-- Reason: WAI's requestBody is an IO action that returns the body as chunks. Once read,
-- they can't be read again. We read them into a lazy Bytestring, so Scotty user can get
-- the raw body, even if they also want to call wai-extra's parsing routines.
parseRequestBody :: MonadIO m
                 => [B.ByteString]
                 -> Parse.BackEnd y
                 -> Request
                 -> m ([Parse.Param], [Parse.File y])
parseRequestBody bl s r =
    case Parse.getRequestBodyType r of
        Nothing -> return ([], [])
        Just rbt -> do
            mvar <- liftIO $ newMVar bl -- MVar is a bit of a hack so we don't have to inline
                                        -- large portions of Network.Wai.Parse
            let provider = modifyMVar mvar $ \bsold -> case bsold of
                                                []     -> return ([], B.empty)
                                                (b:bs) -> return (bs, b)
            liftIO $ Parse.sinkRequestBody s rbt provider

mkEnv :: forall m. MonadIO m => Request -> [Param] -> m ActionEnv
mkEnv req captures = do
    let rbody = requestBody req
        takeAll :: ([B.ByteString] -> m [B.ByteString]) -> m [B.ByteString]
        takeAll prefix = liftIO rbody >>= \ b -> if B.null b then prefix [] else takeAll (prefix . (b:))
    bs <- takeAll return

    (formparams, fs) <- liftIO $ parseRequestBody bs Parse.lbsBackEnd req

    let convert (k, v) = (strictByteStringToLazyText k, strictByteStringToLazyText v)
        parameters = captures ++ map convert formparams ++ queryparams
        queryparams = parseEncodedParams $ rawQueryString req
        b = BL.fromChunks bs

    return $ Env req parameters b [ (strictByteStringToLazyText k, fi) | (k,fi) <- fs ]

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
