{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.Scotty.Types where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Default (Default, def)
import Data.String (IsString(..))
import Data.Text.Lazy (Text, pack)

import qualified Data.Conduit as C
import Network.Wai hiding (Middleware, Application)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (Settings, defaultSettings)
import Network.Wai.Parse (FileInfo)

data Options = Options { verbose :: Int -- ^ 0 = silent, 1(def) = startup banner
                       , settings :: Settings -- ^ Warp 'Settings'
                       }

instance Default Options where
    def = Options 1 defaultSettings

data ScottyState m = ScottyState { middlewares :: [Wai.Middleware]
                                 , routes :: [Middleware m]
                                 }

type Middleware m = Application m -> Application m
type Application m = Request -> C.ResourceT m Response

addMiddleware :: Wai.Middleware -> ScottyState m -> ScottyState m
addMiddleware m s@(ScottyState {middlewares = ms}) = s { middlewares = m:ms }

addRoute :: Monad m => Middleware m -> ScottyState m -> ScottyState m
addRoute r s@(ScottyState {routes = rs}) = s { routes = r:rs }

instance Default (ScottyState m) where
    def = ScottyState [] []

newtype ScottyT m a = ScottyT { runS :: StateT (ScottyState m) m a }
    deriving ( Functor, Applicative, Monad, MonadIO
             , MonadState (ScottyState m))

instance MonadTrans ScottyT where
    lift = ScottyT . lift

type Param = (Text, Text)

data ActionError = Redirect Text
                 | ActionError Text
                 | Next
    deriving (Eq,Show)

instance Error ActionError where
    strMsg = ActionError . pack

type File = (Text, FileInfo ByteString)

data ActionEnv = Env { getReq :: Request, getParams :: [Param], getBody :: ByteString, getFiles :: [File] }

newtype ActionT m a = ActionT { runAM :: ErrorT ActionError (ReaderT ActionEnv (StateT Response m)) a }
    deriving ( Functor, Applicative, Monad, MonadIO
             , MonadReader ActionEnv, MonadState Response, MonadError ActionError)

instance MonadTrans ActionT where
    lift = ActionT . lift . lift . lift

data RoutePattern = Capture   Text
                  | Literal   Text
                  | Function  (Request -> Maybe [Param])

instance IsString RoutePattern where fromString = Capture . pack
