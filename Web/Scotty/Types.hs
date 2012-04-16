{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.Scotty.Types where

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Default (Default, def)
import Data.Text.Lazy (Text, pack)

import Network.Wai

import Data.String (IsString(..))

data ScottyState = ScottyState { middlewares :: [Middleware]
                               , routes :: [Middleware]
                               }

addMiddleware :: Middleware -> ScottyState -> ScottyState
addMiddleware m (ScottyState ms rs) = ScottyState (m:ms) rs

addRoute :: Middleware -> ScottyState -> ScottyState
addRoute r (ScottyState ms rs) = ScottyState ms (r:rs)

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

data ActionEnv = Env { getReq :: Request, getParams :: [Param], getBody :: ByteString }

newtype ActionM a = AM { runAM :: ErrorT ActionError (ReaderT ActionEnv (StateT Response IO)) a }
    deriving ( Monad, MonadIO, Functor
             , MonadReader ActionEnv, MonadState Response, MonadError ActionError)

data RoutePattern = Capture   Text
                  | Literal   Text
                  | Function  (Request -> Maybe [Param])

instance IsString RoutePattern where fromString = Capture . pack
