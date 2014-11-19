{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TypeFamilies, DeriveDataTypeable #-}
module Web.Scotty.Internal.Types where

import           Blaze.ByteString.Builder (Builder)

import           Control.Applicative
import qualified Control.Exception as E
import           Control.Monad.Base (MonadBase, liftBase, liftBaseDefault)
#if MIN_VERSION_mtl(2,2,1)
import           Control.Monad.Except
#else
import           Control.Monad.Error
#endif
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Control (MonadBaseControl, StM, liftBaseWith, restoreM, ComposeSt, defaultLiftBaseWith, defaultRestoreM, MonadTransControl, StT, liftWith, restoreT)

import qualified Data.ByteString as BS
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Default (Default, def)
import           Data.Monoid (mempty)
import           Data.String (IsString(..))
import           Data.Text.Lazy (Text, pack)
import           Data.Typeable (Typeable)

import           Network.HTTP.Types

import           Network.Wai hiding (Middleware, Application)
import qualified Network.Wai as Wai
import           Network.Wai.Handler.Warp (Settings, defaultSettings, setFdCacheDuration)
import           Network.Wai.Parse (FileInfo)

--------------------- Options -----------------------
data Options = Options { verbose :: Int -- ^ 0 = silent, 1(def) = startup banner
                       , settings :: Settings -- ^ Warp 'Settings'
                                              -- Note: to work around an issue in warp,
                                              -- the default FD cache duration is set to 0
                                              -- so changes to static files are always picked
                                              -- up. This likely has performance implications,
                                              -- so you may want to modify this for production
                                              -- servers using `setFdCacheDuration`.
                       }

instance Default Options where
    def = Options 1 (setFdCacheDuration 0 defaultSettings)

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

#if !MIN_VERSION_mtl(2,2,1)
instance ScottyError e => Error (ActionError e) where
    strMsg = stringError
#endif

type ErrorHandler e m = Maybe (e -> ActionT e m ())

------------------ Scotty Actions -------------------
type Param = (Text, Text)

type File = (Text, FileInfo ByteString)

data ActionEnv = Env { getReq       :: Request
                     , getParams    :: [Param]
                     , getBody      :: IO ByteString
                     , getBodyChunk :: IO BS.ByteString
                     , getFiles     :: [File]
                     }

data RequestBodyState = BodyUntouched 
                      | BodyCached ByteString [BS.ByteString] -- whole body, chunks left to stream
                      | BodyCorrupted

data BodyPartiallyStreamed = BodyPartiallyStreamed deriving (Show, Typeable)

instance E.Exception BodyPartiallyStreamed

data Content = ContentBuilder Builder
             | ContentFile    FilePath
             | ContentStream  StreamingBody

data ScottyResponse = SR { srStatus  :: Status
                         , srHeaders :: ResponseHeaders
                         , srContent :: Content
                         }

instance Default ScottyResponse where
    def = SR status200 [] (ContentBuilder mempty)

#if MIN_VERSION_mtl(2,2,1)
newtype ActionT e m a = ActionT { runAM :: ExceptT (ActionError e) (ReaderT ActionEnv (StateT ScottyResponse m)) a }
#else
newtype ActionT e m a = ActionT { runAM :: ErrorT (ActionError e) (ReaderT ActionEnv (StateT ScottyResponse m)) a }
#endif
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


instance (MonadBase b m, ScottyError e) => MonadBase b (ActionT e m) where
    liftBase = liftBaseDefault


instance ScottyError e => MonadTransControl (ActionT e) where
#if MIN_VERSION_mtl(2,2,1)
     newtype StT (ActionT e) a = StAction {unStAction :: StT (StateT ScottyResponse) (StT (ReaderT ActionEnv) (StT (ExceptT (ActionError e)) a))}
#else
     newtype StT (ActionT e) a = StAction {unStAction :: StT (StateT ScottyResponse) (StT (ReaderT ActionEnv) (StT (ErrorT (ActionError e)) a))}
#endif
     liftWith = \f ->
        ActionT $  liftWith $ \run  ->
                   liftWith $ \run' ->
                   liftWith $ \run'' ->
                   f $ liftM StAction . run'' . run' . run . runAM
     restoreT = ActionT . restoreT . restoreT . restoreT . liftM unStAction

instance (ScottyError e, MonadBaseControl b m) => MonadBaseControl b (ActionT e m) where
    newtype StM (ActionT e m) a = STMAction {unStMActionT :: ComposeSt (ActionT e) m a}
    liftBaseWith = defaultLiftBaseWith STMAction
    restoreM     = defaultRestoreM   unStMActionT

------------------ Scotty Routes --------------------
data RoutePattern = Capture   Text
                  | Literal   Text
                  | Function  (Request -> Maybe [Param])

instance IsString RoutePattern where
    fromString = Capture . pack
