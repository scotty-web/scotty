{-# LANGUAGE GeneralizedNewtypeDeriving, InstanceSigs, MultiParamTypeClasses #-}
module Web.Scotty.Types where

import           Blaze.ByteString.Builder (Builder)

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.Conduit as C
import           Data.Default (Default, def)
import           Data.Monoid (mempty)
import           Data.String (IsString(..))
import           Data.Text.Lazy (Text, pack)

import           Network.HTTP.Types

import           Network.Wai hiding (Middleware, Application)
import qualified Network.Wai as Wai
import           Network.Wai.Handler.Warp (Settings, defaultSettings)
import           Network.Wai.Parse (FileInfo)

--------------------- Options -----------------------
data Options = Options { verbose :: Int -- ^ 0 = silent, 1(def) = startup banner
                       , settings :: Settings -- ^ Warp 'Settings'
                       }

instance Default Options where
    def = Options 1 defaultSettings

----- Transformer Aware Applications/Middleware -----
type Middleware m = Application m -> Application m
type Application m = Request -> m Response

--------------- Scotty Applications -----------------
data ScottyState m = ScottyState { middlewares :: [Wai.Middleware]
                                 , routes :: [Middleware m]
                                 }

instance Default (ScottyState m) where
    def = ScottyState [] []

addMiddleware :: Wai.Middleware -> ScottyState m -> ScottyState m
addMiddleware m s@(ScottyState {middlewares = ms}) = s { middlewares = m:ms }

addRoute :: Monad m => Middleware m -> ScottyState m -> ScottyState m
addRoute r s@(ScottyState {routes = rs}) = s { routes = r:rs }

newtype ScottyT m a = ScottyT { runS :: StateT (ScottyState m) m a }
    deriving ( Functor, Applicative, Monad, MonadIO )

instance MonadTrans ScottyT where
    lift = ScottyT . lift

------------------ Scotty Actions -------------------
type Param = (Text, Text)

data ActionError e = Redirect Text
                   | StringError Text
                   | Next
                   | ActionError e

instance Error (ActionError e) where
    strMsg = StringError . pack

type File = (Text, FileInfo ByteString)

data ActionEnv = Env { getReq    :: Request
                     , getParams :: [Param]
                     , getBody   :: ByteString
                     , getFiles  :: [File] 
                     }

data Content = ContentBuilder Builder
             | ContentFile    FilePath
             | ContentSource  (C.Source IO (C.Flush Builder))

data ScottyResponse = SR { srStatus  :: Status
                         , srHeaders :: ResponseHeaders
                         , srContent :: Content
                         }

instance Default ScottyResponse where
    def = SR status200 [] (ContentBuilder mempty)

newtype ActionT e m a = ActionT { runAM :: ErrorT (ActionError e) (ReaderT ActionEnv (StateT ScottyResponse m)) a }
    deriving ( Functor, Applicative, Monad, MonadIO )

instance MonadTrans (ActionT e) where
    lift = ActionT . lift . lift . lift

instance Monad m => MonadError (ActionError e) (ActionT e m) where
    throwError :: ActionError e -> ActionT e m a
    throwError = ActionT . throwError

    catchError :: ActionT e m a -> (ActionError e -> ActionT e m a) -> ActionT e m a
    catchError (ActionT m) f = ActionT (catchError m (runAM . f))

------------------ Scotty Routes --------------------
data RoutePattern = Capture   Text
                  | Literal   Text
                  | Function  (Request -> Maybe [Param])

instance IsString RoutePattern where 
    fromString = Capture . pack
