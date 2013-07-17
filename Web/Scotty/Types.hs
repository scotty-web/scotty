{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.Scotty.Types where

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Morph
import Control.Monad.Trans.Resource (ResourceT)

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

data ScottyStateT m = ScottyState { middlewares :: [Middleware]
                                  , routes :: [MiddlewareT m]
                                  }
                      
type ScottyState = ScottyStateT IO                      

addMiddleware :: Middleware -> ScottyStateT m -> ScottyStateT m
addMiddleware m s@(ScottyState {middlewares = ms}) = s { middlewares = m:ms }

liftApp :: (Monad m, MonadIO m) => Application -> ApplicationT m
liftApp app = hoist liftIO . app
        
addRoute :: MiddlewareT m -> ScottyStateT m -> ScottyStateT m
addRoute r s@(ScottyState {routes = rs}) = s { routes = r:rs }

instance Default (ScottyStateT m) where
    def = ScottyState [] []

newtype ScottyT m a = ScottyT {
   runScottyT :: StateT (ScottyStateT m) m a
  } deriving (Monad, MonadIO, Functor, MonadState (ScottyStateT m))

instance MonadTrans ScottyT where
  lift = ScottyT . lift
             
type ScottyM a = ScottyT IO a

runS :: ScottyM a -> StateT ScottyState IO a
runS = runScottyT

type Param = (Text, Text)

data ActionError = Redirect Text
                 | ActionError Text
                 | Next
    deriving (Eq,Show)

instance Error ActionError where
    strMsg = ActionError . pack

type File = (Text, FileInfo ByteString)

data ActionEnv = Env { getReq :: Request, getParams :: [Param], getBody :: ByteString, getFiles :: [File] }

newtype ActionT m a = AT {
  runAT :: ErrorT ActionError (ReaderT ActionEnv (StateT Response m)) a
  } deriving ( Monad, MonadIO, Functor
             , MonadReader ActionEnv, MonadState Response
             , MonadError ActionError)

instance MonadTrans ActionT where
  lift = AT . lift . lift . lift

type ActionM a = ActionT IO a

runAM :: ActionM a
      -> ErrorT ActionError (ReaderT ActionEnv (StateT Response IO)) a
runAM = runAT
    
data RoutePattern = Capture   Text
                  | Literal   Text
                  | Function  (Request -> Maybe [Param])

instance IsString RoutePattern where fromString = Capture . pack

type MiddlewareT m = Application -> ApplicationT m
type ApplicationT m = Request -> ResourceT m Response
