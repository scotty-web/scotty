{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# options_ghc -Wno-unused-imports #-}
module Web.Scotty.Internal.Types where

import           Blaze.ByteString.Builder (Builder)

import           Control.Applicative
import Control.Concurrent.MVar
import Control.Concurrent.STM (STM, TVar, atomically, newTVarIO, readTVarIO, readTVar, writeTVar, modifyTVar')
import qualified Control.Exception as E
import qualified Control.Monad as Monad
import           Control.Monad (MonadPlus(..))
import           Control.Monad.Base (MonadBase, liftBase, liftBaseDefault)
import           Control.Monad.Catch (MonadCatch, MonadThrow)
import           Control.Monad.Error.Class
import qualified Control.Monad.Fail as Fail
import           Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import           Control.Monad.Reader (MonadReader(..), ReaderT, mapReaderT, asks)
import           Control.Monad.State.Strict (MonadState(..), State, StateT, mapStateT)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Control (MonadBaseControl, StM, liftBaseWith, restoreM, ComposeSt, defaultLiftBaseWith, defaultRestoreM, MonadTransControl, StT, liftWith, restoreT)
import           Control.Monad.Trans.Except


import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS8 (ByteString)
-- import           Data.Default.Class (Default, def)
import           Data.String (IsString(..))
import           Data.Text.Lazy (Text, pack)
import           Data.Typeable (Typeable)

import           Network.HTTP.Types

import           Network.Wai hiding (Middleware, Application)
import qualified Network.Wai as Wai
import           Network.Wai.Handler.Warp (Settings, defaultSettings)
import           Network.Wai.Parse (FileInfo)

import           Prelude ()
import "base-compat-batteries" Prelude.Compat

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

defaultOptions :: Options
defaultOptions = Options 1 defaultSettings

newtype RouteOptions = RouteOptions { maxRequestBodySize :: Maybe Kilobytes -- max allowed request size in KB
                                    }

defaultRouteOptions :: RouteOptions
defaultRouteOptions = RouteOptions Nothing

type Kilobytes = Int
----- Transformer Aware Applications/Middleware -----
type Middleware m = Application m -> Application m
type Application m = Request -> m Response

------------------ Scotty Request Body --------------------

data BodyChunkBuffer = BodyChunkBuffer { hasFinishedReadingChunks :: Bool -- ^ whether we've reached the end of the stream yet
                                       , chunksReadSoFar :: [BS.ByteString]
                                       }
-- | The key part of having two MVars is that we can "clone" the BodyInfo to create a copy where the index is reset to 0, but the chunk cache is the same. Passing a cloned BodyInfo into each matched route allows them each to start from the first chunk if they call bodyReader.
--
-- Introduced in (#308)
data BodyInfo = BodyInfo { bodyInfoReadProgress :: MVar Int -- ^ index into the stream read so far
                         , bodyInfoChunkBuffer :: MVar BodyChunkBuffer
                         , bodyInfoDirectChunkRead :: IO BS.ByteString -- ^ can be called to get more chunks
                         }

--------------- Scotty Applications -----------------

data ScottyState m =
    ScottyState { middlewares :: [Wai.Middleware]
                , routes :: [BodyInfo -> Middleware m]
                , handler :: Maybe (ErrorHandler m) -- Maybe (AErr -> ActionT m ()) -- ErrorHandler e m
                , routeOptions :: RouteOptions
                }

defaultScottyState :: ScottyState m
defaultScottyState = ScottyState [] [] Nothing defaultRouteOptions

addMiddleware :: Wai.Middleware -> ScottyState m -> ScottyState m
addMiddleware m s@(ScottyState {middlewares = ms}) = s { middlewares = m:ms }

addRoute :: (BodyInfo -> Middleware m) -> ScottyState m -> ScottyState m
addRoute r s@(ScottyState {routes = rs}) = s { routes = r:rs }

setHandler :: Maybe (ErrorHandler m) -> ScottyState m -> ScottyState m
setHandler h s = s { handler = h }

updateMaxRequestBodySize :: RouteOptions -> ScottyState m -> ScottyState m
updateMaxRequestBodySize RouteOptions { .. } s@ScottyState { routeOptions = ro } =
    let ro' = ro { maxRequestBodySize = maxRequestBodySize }
    in s { routeOptions = ro' }

newtype ScottyT m a = ScottyT { runS :: State (ScottyState m) a }
    deriving ( Functor, Applicative, Monad )


------------------ Scotty Errors --------------------

data ActionError
  = Redirect Text
  | Next
  | Finish
  | StatusError Status Text -- e.g. 422 Unprocessable Entity when JSON body parsing fails
  -- | SomeActionError Status AErr
  deriving (Show, Typeable)
instance E.Exception ActionError

-- actionErrorHandler = Handler (\(e1 :: ActionError e))

-- | Handler for a specific type of exception, see 'handleActionError'
--
-- TODO export the constructor to the user since they will need to implement their
-- own handler
data Handler m a = forall e . E.Exception e => Handler (e -> m a)
instance Monad m => Functor (Handler m) where
  fmap f (Handler h) = Handler (\e -> f <$> h e)



-- -- | FIXME placeholder for a more informative concrete error type
-- newtype AErr = AErr { aErrText :: Text } deriving (Show, Typeable)
-- instance E.Exception AErr
-- instance IsString AErr where
--   fromString = AErr . pack


-- -- | In order to use a custom exception type (aside from 'Text'), you must
-- -- define an instance of 'ScottyError' for that type.
-- class ScottyError e where
--     stringError :: String -> e
--     showError :: e -> Text

type ErrorHandler m = Handler (ActionT m) ()
-- type ErrorHandler e m = Maybe (e -> ActionT e m ())

data ScottyException = RequestException BS.ByteString Status deriving (Show, Typeable)
instance E.Exception ScottyException

------------------ Scotty Actions -------------------
type Param = (Text, Text)

type File = (Text, FileInfo LBS8.ByteString)

data ActionEnv = Env { envReq       :: Request
                     , envCaptureParams :: [Param]
                     , envFormParams    :: [Param]
                     , envQueryParams :: [Param]
                     , envBody      :: IO LBS8.ByteString
                     , envBodyChunk :: IO BS.ByteString
                     , envFiles     :: [File]
                     , envResponse :: TVar ScottyResponse
                     }

getResponse :: MonadIO m => ActionEnv -> m ScottyResponse
getResponse ae = liftIO $ readTVarIO (envResponse ae)

modifyResponse :: (MonadIO m) => (ScottyResponse -> ScottyResponse) -> ActionT m ()
modifyResponse f = do
  tv <- asks envResponse
  liftIO $ atomically $ modifyTVar' tv f

data BodyPartiallyStreamed = BodyPartiallyStreamed deriving (Show, Typeable)

instance E.Exception BodyPartiallyStreamed

data Content = ContentBuilder  Builder
             | ContentFile     FilePath
             | ContentStream   StreamingBody
             | ContentResponse Response

data ScottyResponse = SR { srStatus  :: Status
                         , srHeaders :: ResponseHeaders
                         , srContent :: Content
                         }

setContent :: Content -> ScottyResponse -> ScottyResponse
setContent c sr = sr { srContent = c }

setHeaderWith :: ([(HeaderName, BS.ByteString)] -> [(HeaderName, BS.ByteString)]) -> ScottyResponse -> ScottyResponse
setHeaderWith f sr = sr { srHeaders = f (srHeaders sr) }

setStatus :: Status -> ScottyResponse -> ScottyResponse
setStatus s sr = sr { srStatus = s }

defaultScottyResponse :: ScottyResponse
defaultScottyResponse = SR status200 [] (ContentBuilder mempty)


newtype ActionT m a = ActionT { runAM :: ReaderT ActionEnv m a }
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadIO, MonadReader ActionEnv, MonadTrans, MonadThrow, MonadCatch, MonadBase b, MonadBaseControl b, MonadTransControl, MonadUnliftIO)

instance (Semigroup a) => Semigroup (ScottyT m a) where
  x <> y = (<>) <$> x <*> y

instance
  ( Monoid a
#if !(MIN_VERSION_base(4,11,0))
  , Semigroup a
#endif
#if !(MIN_VERSION_base(4,8,0))
  , Functor m
#endif
  ) => Monoid (ScottyT m a) where
  mempty = return mempty
#if !(MIN_VERSION_base(4,11,0))
  mappend = (<>)
#endif

instance
  ( Monad m
#if !(MIN_VERSION_base(4,8,0))
  , Functor m
#endif
  , Semigroup a
  ) => Semigroup (ActionT m a) where
  x <> y = (<>) <$> x <*> y

instance
  ( Monad m, Monoid a
#if !(MIN_VERSION_base(4,11,0))
  , Semigroup a
#endif
#if !(MIN_VERSION_base(4,8,0))
  , Functor m
#endif
  ) => Monoid (ActionT m a) where
  mempty = return mempty
#if !(MIN_VERSION_base(4,11,0))
  mappend = (<>)
#endif

------------------ Scotty Routes --------------------
data RoutePattern = Capture   Text
                  | Literal   Text
                  | Function  (Request -> Maybe [Param])

instance IsString RoutePattern where
    fromString = Capture . pack



