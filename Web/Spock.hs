{-# LANGUAGE OverloadedStrings #-}
module Web.Spock
    ( spock
    , middleware
    , get, post, put, delete, addroute
    , text, html, file, json
    , request, params, param
    , runAction -- remove
    ) where

import Blaze.ByteString.Builder (fromByteString, fromLazyByteString)

import Control.Applicative
import Control.Monad.Reader
import qualified Control.Monad.State as MS

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
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

type SpockT a = MS.StateT SpockState IO a
type SpockM = SpockT ()

spock :: Port -> SpockM -> IO ()
spock p app = do
    putStrLn "Setting phasers to stun..."
    ks <- MS.execStateT app def
    run p $ foldr ($) notFoundApp $ middlewares ks ++ routes ks

notFoundApp :: Application
notFoundApp _ = return $ setStatus status404 $ htmlResponse "404: File Not Found!"

middleware :: Middleware -> SpockM
middleware m = MS.modify (\ (SpockState ms rs) -> SpockState (m:ms) rs)

type Param = (T.Text, T.Text)

type ActionT = ReaderT (Request,[Param]) (MS.StateT Response IO)
type ActionM = ActionT ()

request :: ActionT Request
request = fst <$> ask

params :: ActionT [Param]
params = snd <$> ask

param :: T.Text -> ActionT (Maybe T.Text)
param k = lookup k <$> params

runAction :: [Param] -> ActionM -> Application
runAction ps action req = lift $ flip MS.execStateT def $ runReaderT action (req,ps)

get, post, put, delete :: BS.ByteString -> ActionM -> SpockM
get    = addroute GET
post   = addroute POST
put    = addroute PUT
delete = addroute DELETE

addroute :: StdMethod -> BS.ByteString -> ActionM -> SpockM
addroute method path action = MS.modify (\ (SpockState ms rs) -> SpockState ms (r:rs))
    where r = route method path action

-- todo: captures and wildcards
route :: StdMethod -> BS.ByteString -> ActionM -> Middleware
route method path action app req =
    if Right method == parseMethod (requestMethod req)
    then case matchRoute path (rawPathInfo req) of
            Just params -> runAction (addQueryParams req params) action req
            Nothing -> app req
    else app req

addQueryParams req = (++ [ (k, fromMaybe "" v) | (k,v) <- parseQueryText (rawQueryString req) ])

matchRoute :: BS.ByteString -> BS.ByteString -> Maybe [Param]
matchRoute pat req = go (BS.split '/' pat) (BS.split '/' req) []
    where go [] [] ps = Just ps -- request string and pattern match!
          go [] r  ps | BS.null (mconcat r)  = Just ps -- in case request has trailing slashes
                      | otherwise            = Nothing -- request string is longer than pattern
          go p  [] ps | BS.null (mconcat p)  = Just ps -- in case pattern has trailing slashes
                      | otherwise            = Nothing -- request string is not long enough
          go (p:ps) (r:rs) prs | p == r           = go ps rs prs -- equal literals, keeping checking
                               | BS.null p        = Nothing      -- p is null, but r is not, fail
                               | BS.head p == ':' = go ps rs $ (E.decodeUtf8 $ BS.tail p, E.decodeUtf8 r) : prs -- p is a capture, add to params
                               | otherwise        = Nothing      -- both literals, but unequal, fail

text :: T.Text -> ActionM
text = MS.put . textResponse

html :: T.Text -> ActionM
html t = text t >> MS.modify (setHeader ("Content-Type", "text/html"))

file :: FilePath -> ActionM
file = MS.put . fileResponse

json :: (A.ToJSON a) => a -> ActionM
json val = MS.put $ ResponseBuilder status200
                                    [ ("Content-Type", "application/json") ]
                                    $ fromLazyByteString $ A.encode val

-- TODO: look up MIME type based on extension
fileResponse :: FilePath -> Response
fileResponse fp = ResponseFile status200 [ ("Content-Type", "text/html") ] fp Nothing

textResponse :: T.Text -> Response
textResponse t = ResponseBuilder status200
                                 [ ("Content-Type", "text/plain"), ("Content-Length", BS.pack $ show $ T.length t) ]
                                 $ fromByteString $ E.encodeUtf8 t

htmlResponse :: T.Text -> Response
htmlResponse = setHeader ("Content-Type", "text/html") . textResponse

