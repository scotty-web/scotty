{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.Scotty.Types where

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Default (Default, def)
import Data.String (IsString(..))
import Data.Text.Lazy (Text, pack)

import Network.Wai
import Network.Wai.Handler.Warp (Settings, defaultSettings)
import Network.Wai.Parse (FileInfo)

data Options = Options { verbose :: Int -- ^ 0 = silent, 1(def) = startup banner
                       , settings :: Settings -- ^ Warp 'Settings'
                       }

instance Default Options where
    def = Options 1 defaultSettings

data ScottyState = ScottyState { middlewares :: [Middleware]
                               , routes :: [Middleware]
                               }

addMiddleware :: Middleware -> ScottyState -> ScottyState
addMiddleware m s@(ScottyState {middlewares = ms}) = s { middlewares = m:ms }

addRoute :: Middleware -> ScottyState -> ScottyState
addRoute r s@(ScottyState {routes = rs}) = s { routes = r:rs }

instance Default ScottyState where
    def = ScottyState [] []

newtype ScottyM a = S { runS :: StateT ScottyState IO a }
    deriving (Monad, MonadIO, Functor, MonadState ScottyState)

type Param = (Text, Text)

data ActionError = Redirect Text
                 | ActionError Text
                 | Next
    deriving (Eq,Show)

instance Error ActionError where
    strMsg = ActionError . pack

type File = (Text, FileInfo ByteString)

data ActionEnv = Env { getReq :: Request, getParams :: [Param], getBody :: ByteString, getFiles :: [File] }

newtype ActionM a = AM { runAM :: ErrorT ActionError (ReaderT ActionEnv (StateT Response IO)) a }
    deriving ( Monad, MonadIO, Functor
             , MonadReader ActionEnv, MonadState Response, MonadError ActionError)

data RoutePattern = Capture   Text
                  | Literal   Text
                  | Function  (Request -> Maybe [Param])

instance IsString RoutePattern where fromString = Capture . pack
