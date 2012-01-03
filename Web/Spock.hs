{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Web.Spock
    ( spock, spockApp -- spock-to-WAI
    , SpockM, ActionM -- Types
    , middleware, get, post, put, delete, addroute -- building apps
    , request, params, param -- getting input
    , status, header, redirect -- editing response and redirect
    , text, html, file, json -- setting output
    , raise, rescue          -- error handling
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

spock :: Port -> SpockM () -> IO ()
spock p s = putStrLn "Setting phasers to stun..." >> (run p =<< spockApp s)

spockApp :: SpockM () -> IO Application
spockApp defs = do
    s <- MS.execStateT (runS defs) def
    return $ foldl (flip ($)) notFoundApp $ middlewares s ++ routes s

notFoundApp :: Application
notFoundApp _ = return $ ResponseBuilder status404 [("Content-Type","text/html")]
                       $ fromByteString "<h1>404: File Not Found!</h1>"

middleware :: Middleware -> SpockM ()
middleware m = MS.modify (\ (SpockState ms rs) -> SpockState (m:ms) rs)

type Param = (T.Text, T.Text)

data ActionError = Redirect T.Text
                 | ActionError T.Text
    deriving (Eq,Show)

instance Error ActionError where
    strMsg = ActionError . T.pack

newtype ActionM a = AM { runAM :: ErrorT ActionError (ReaderT (Request,[Param]) (MS.StateT Response IO)) a }
    deriving ( Monad, MonadIO, MonadFix, Functor
             , MonadReader (Request,[Param]), MS.MonadState Response, MonadError ActionError)

runAction :: [Param] -> ActionM () -> Application
runAction ps action req = lift $ flip MS.execStateT def $ runReaderT (runErrorT $ runAM $ handleErrors action) (req,ps)

handleErrors :: ActionM () -> ActionM ()
handleErrors action = catchError action $ \e ->
    case e of
        Redirect url -> do
            status status302
            header "Location" url
        ActionError msg -> do
            html $ mconcat ["<h1>500 Internal Server Error</h1>", msg]
            status status500

raise :: T.Text -> ActionM a
raise = throwError . ActionError

rescue :: ActionM a -> (T.Text -> ActionM a) -> ActionM a
rescue action handler = catchError action $ \e -> case e of
    ActionError msg -> handler msg  -- handle errors
    r               -> throwError r -- rethrow redirects

request :: ActionM Request
request = fst <$> ask

params :: ActionM [Param]
params = snd <$> ask

param :: T.Text -> ActionM T.Text
param k = do
    val <- lookup k <$> params
    maybe (raise $ mconcat ["Param: ", k, " not found!"]) return val

get, post, put, delete :: T.Text -> ActionM () -> SpockM ()
get    = addroute GET
post   = addroute POST
put    = addroute PUT
delete = addroute DELETE

addroute :: StdMethod -> T.Text -> ActionM () -> SpockM ()
addroute method path action = MS.modify (\ (SpockState ms rs) -> SpockState ms (r:rs))
    where r = route method path action

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
text = MS.put . textResponse

html :: T.Text -> ActionM ()
html t = text t >> MS.modify (setHeader ("Content-Type", "text/html"))

file :: FilePath -> ActionM ()
file = MS.put . fileResponse

json :: (A.ToJSON a) => a -> ActionM ()
json val = MS.put $ ResponseBuilder status200
                                    [ ("Content-Type", "application/json") ]
                                    $ fromLazyByteString $ A.encode val

-- TODO: look up MIME type based on extension?
fileResponse :: FilePath -> Response
fileResponse fp = ResponseFile status200 [] fp Nothing

textResponse :: T.Text -> Response
textResponse t = ResponseBuilder status200
                                 [ ("Content-Type", "text/plain"), ("Content-Length", BS.pack $ show $ T.length t) ]
                                 $ fromByteString $ E.encodeUtf8 t

htmlResponse :: T.Text -> Response
htmlResponse = setHeader ("Content-Type", "text/html") . textResponse

