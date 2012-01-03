{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Web.Spock
    ( -- * spock-to-WAI
      spock, spockApp
      -- * Defining Middleware and Routes
      --
      -- | 'Middleware' and routes are run in the order in which they
      -- are defined. All middleware is run first, followed by all routes.
      -- If no route matches, a 404 response is given.
    , middleware, get, post, put, delete, addroute
      -- * Defining Actions
      -- ** Accessing the Request, Captures, and Query Parameters
    , request, param
      -- ** Modifying the Response and Redirecting
    , status, header, redirect
      -- ** Setting Response
    , text, html, file, json
      -- ** Exceptions
    , raise, rescue
      -- * Types
    , SpockM, ActionM
    ) where

import Blaze.ByteString.Builder (fromByteString, fromLazyByteString)

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import qualified Control.Monad.State as MS

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI
import Data.Default
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

import Network.HTTP.Types

import Network.Wai
import Network.Wai.Handler.Warp -- for 'run'
import Network.Wai.Util

data SpockState = SpockState {
        middlewares :: [Middleware],
        routes :: [Middleware]
    }

instance Default SpockState where
    def = SpockState [] []

newtype SpockM a = S { runS :: MS.StateT SpockState IO a }
    deriving (Monad, Functor, MS.MonadState SpockState)

-- | Run a spock application using the warp server.
spock :: Port -> SpockM () -> IO ()
spock p s = putStrLn "Setting phasers to stun..." >> (run p =<< spockApp s)

-- | Turn a spock application into a WAI 'Application', which can be
-- run with any WAI handler.
spockApp :: SpockM () -> IO Application
spockApp defs = do
    s <- MS.execStateT (runS defs) def
    return $ foldl (flip ($)) notFoundApp $ middlewares s ++ routes s

notFoundApp :: Application
notFoundApp _ = return $ ResponseBuilder status404 [("Content-Type","text/html")]
                       $ fromByteString "<h1>404: File Not Found!</h1>"

-- | Use given middleware. Middleware is nested such that the first declared
-- is G
middleware :: Middleware -> SpockM ()
middleware m = MS.modify (\ (SpockState ms rs) -> SpockState (m:ms) rs)

type Param = (T.Text, T.Text)

data ActionError = Redirect T.Text
                 | ActionError T.Text
    deriving (Eq,Show)

instance Error ActionError where
    strMsg = ActionError . T.pack

newtype ActionM a = AM { runAM :: ErrorT ActionError (ReaderT (Request,[Param]) (MS.StateT Response IO)) a }
    deriving ( Monad, MonadIO, Functor
             , MonadReader (Request,[Param]), MS.MonadState Response, MonadError ActionError)

runAction :: [Param] -> ActionM () -> Application
runAction ps action req = lift
                        $ flip MS.execStateT def
                        $ flip runReaderT (req,ps)
                        $ runErrorT
                        $ runAM
                        $ action `catchError` defaultHandler

defaultHandler :: ActionError -> ActionM ()
defaultHandler (Redirect url) = do
    status status302
    header "Location" url
defaultHandler (ActionError msg) = do
    status status500
    html $ mconcat ["<h1>500 Internal Server Error</h1>", msg]

raise :: T.Text -> ActionM a
raise = throwError . ActionError

rescue :: ActionM a -> (T.Text -> ActionM a) -> ActionM a
rescue action handler = catchError action $ \e -> case e of
    ActionError msg -> handler msg  -- handle errors
    r               -> throwError r -- rethrow redirects

request :: ActionM Request
request = fst <$> ask

param :: T.Text -> ActionM T.Text
param k = do
    val <- lookup k <$> snd <$> ask
    maybe (raise $ mconcat ["Param: ", k, " not found!"]) return val

get, post, put, delete :: T.Text -> ActionM () -> SpockM ()
get    = addroute GET
post   = addroute POST
put    = addroute PUT
delete = addroute DELETE

addroute :: StdMethod -> T.Text -> ActionM () -> SpockM ()
addroute method path action = MS.modify (\ (SpockState ms rs) -> SpockState ms (r:rs))
    where r = route method withSlash action
          withSlash = case T.uncons path of
                        Just ('/',_) -> path
                        _            -> T.cons '/' path

-- todo: wildcards?
route :: StdMethod -> T.Text -> ActionM () -> Middleware
route method path action app req =
    if Right method == parseMethod (requestMethod req)
    then case matchRoute path (E.decodeUtf8 $ rawPathInfo req) of
            Just params -> runAction (addQueryParams req params) action req
            Nothing -> app req
    else app req

addQueryParams req = (++ [ (k, fromMaybe "" v) | (k,v) <- parseQueryText (rawQueryString req) ])

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

status :: Status -> ActionM ()
status = MS.modify . setStatus

header :: T.Text -> T.Text -> ActionM ()
header k v = MS.modify $ setHeader (CI.mk $ E.encodeUtf8 k, E.encodeUtf8 v)

redirect :: T.Text -> ActionM ()
redirect = throwError . Redirect

text :: T.Text -> ActionM ()
text t = do
    header "Content-Type" "text/plain"
    MS.modify $ setContent $ Left $ fromByteString $ E.encodeUtf8 t

html :: T.Text -> ActionM ()
html t = do
    header "Content-Type" "text/html"
    MS.modify $ setContent $ Left $ fromByteString $ E.encodeUtf8 t

file :: FilePath -> ActionM ()
file = MS.modify . setContent . Right

json :: (A.ToJSON a) => a -> ActionM ()
json v = do
    header "Content-Type" "application/json"
    MS.modify $ setContent $ Left $ fromLazyByteString $ A.encode v
