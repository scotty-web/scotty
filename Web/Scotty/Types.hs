{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-}
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
import qualified Control.Exception as E

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
data ScottyState e m = 
    ScottyState { middlewares :: [Wai.Middleware]
                , routes :: [Middleware m]
                , handler :: ErrorHandler e m
                }

instance Monad m => Default (ScottyState e m) where
    def = ScottyState [] [] Nothing

addMiddleware :: Wai.Middleware -> ScottyState e m -> ScottyState e m
addMiddleware m s@(ScottyState {middlewares = ms}) = s { middlewares = m:ms }

addRoute :: Monad m => Middleware m -> ScottyState e m -> ScottyState e m
addRoute r s@(ScottyState {routes = rs}) = s { routes = r:rs }

addHandler :: ErrorHandler e m -> ScottyState e m -> ScottyState e m
addHandler h s = s { handler = h }

newtype ScottyT e m a = ScottyT { runS :: StateT (ScottyState e m) m a }
    deriving ( Functor, Applicative, Monad, MonadIO )

instance MonadTrans (ScottyT e) where
    lift = ScottyT . lift

------------------ Scotty Errors --------------------
data ActionError e = Redirect Text
                   | Next
                   | ActionError e

-- | In order to use a custom exception type (aside from 'Text'), you must
-- define an instance of 'ScottyError' for that type. 
class ScottyError e where
    stringError :: String -> e
    showError :: e -> Text

instance ScottyError Text where
    stringError = pack
    showError = id

instance ScottyError e => ScottyError (ActionError e) where
    stringError = ActionError . stringError
    showError (Redirect url)  = url
    showError Next            = pack "Next"
    showError (ActionError e) = showError e

instance ScottyError e => Error (ActionError e) where 
    strMsg = stringError

type ErrorHandler e m = Maybe (e -> ActionT e m ())

------------------ Scotty Actions -------------------
type Param = (Text, Text)

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
    deriving ( Functor, Applicative, Monad )

instance (MonadIO m, ScottyError e) => MonadIO (ActionT e m) where
    liftIO io = ActionT $ do
                    r <- liftIO $ liftM Right io `E.catch` (\ e -> return $ Left $ stringError $ show (e :: E.SomeException))
                    either throwError return r

instance ScottyError e => MonadTrans (ActionT e) where
    lift = ActionT . lift . lift . lift

instance (ScottyError e, Monad m) => MonadError (ActionError e) (ActionT e m) where
    throwError = ActionT . throwError

    catchError (ActionT m) f = ActionT (catchError m (runAM . f))

------------------ Scotty Routes --------------------
data RoutePattern = Capture   Text
                  | Literal   Text
                  | Function  (Request -> Maybe [Param])

instance IsString RoutePattern where 
    fromString = Capture . pack
