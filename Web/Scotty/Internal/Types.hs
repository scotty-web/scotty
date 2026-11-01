{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Scotty.Internal.Types where

import           Blaze.ByteString.Builder (Builder)

import           Control.Applicative
import Control.Concurrent.MVar
import Control.Concurrent.STM (TVar, atomically, readTVarIO, modifyTVar')
import qualified Control.Exception as E
import           Control.Monad (MonadPlus(..))
import           Control.Monad.Base (MonadBase)
import           Control.Monad.Catch (MonadCatch, MonadThrow)
import           Control.Monad.IO.Class (MonadIO(..))
import UnliftIO (MonadUnliftIO(..))
import           Control.Monad.Reader (MonadReader(..), ReaderT, asks, mapReaderT)
import           Control.Monad.State.Strict (State)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Control (MonadBaseControl, MonadTransControl)
import qualified Control.Monad.Trans.Resource as RT (InternalState, InvalidAccess)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS8 (ByteString)
import           Data.String (IsString(..))
import qualified Data.Text as T (Text, pack)
import           GHC.Stack (callStack)
import           Network.HTTP.Types

import           Network.Wai hiding (Middleware, Application)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as W (Settings, defaultSettings, InvalidRequest(..))
import           Network.Wai.Parse (FileInfo)
import qualified Network.Wai.Parse as WPS (ParseRequestBodyOptions, RequestParseException(..))

import UnliftIO.Exception (Handler(..), catch, catches, StringException(..))




--------------------- Docs & OpenAPI -----------------------

-- | Configuration for automatically generated Swagger UI and OpenAPI documentation.
data DocsConfig = DocsConfig
    { docsPath          :: T.Text       -- ^ Path to serve Swagger UI, default: "/docs"
    , openApiPath       :: T.Text       -- ^ Path to serve OpenAPI JSON spec, default: "/openapi.json"
    , docsTitle         :: T.Text       -- ^ Title displayed in Swagger UI and OpenAPI spec
    , docsVersion       :: T.Text       -- ^ API version string
    , docsDescription   :: Maybe T.Text -- ^ Optional API description
    , docsSwaggerCssUrl :: T.Text       -- ^ URL to Swagger UI stylesheet
    , docsSwaggerJsUrl  :: T.Text       -- ^ URL to Swagger UI bundle script
    } deriving (Eq, Show)

-- | Default Swagger UI / OpenAPI configuration.
defaultDocsConfig :: DocsConfig
defaultDocsConfig = DocsConfig
    { docsPath          = "/docs"
    , openApiPath       = "/openapi.json"
    , docsTitle         = "Scotty API"
    , docsVersion       = "0.1.0"
    , docsDescription   = Just "Interactive API documentation powered by Scotty & Swagger UI"
    , docsSwaggerCssUrl = "https://cdn.jsdelivr.net/npm/swagger-ui-dist@5/swagger-ui.css"
    , docsSwaggerJsUrl  = "https://cdn.jsdelivr.net/npm/swagger-ui-dist@5/swagger-ui-bundle.js"
    }

-- | OpenAPI parameter location.
data ParamIn = ParamPath | ParamQuery | ParamHeader | ParamCookie
    deriving (Eq, Show)

-- | Documentation for a parameter.
data ParamDoc = ParamDoc
    { pdName        :: T.Text
    , pdIn          :: ParamIn
    , pdRequired    :: Bool
    , pdDescription :: Maybe T.Text
    , pdSchemaType  :: T.Text -- ^ e.g. "string", "integer", "boolean"
    } deriving (Eq, Show)

-- | Metadata describing a route in OpenAPI / Swagger UI.
data RouteDoc = RouteDoc
    { rdSummary     :: Maybe T.Text
    , rdDescription :: Maybe T.Text
    , rdTags        :: [T.Text]
    , rdParameters  :: [ParamDoc]
    , rdDeprecated  :: Bool
    , rdOperationId :: Maybe T.Text
    } deriving (Eq, Show)

-- | Default empty route documentation.
defaultRouteDoc :: RouteDoc
defaultRouteDoc = RouteDoc Nothing Nothing [] [] False Nothing

instance Semigroup RouteDoc where
    d1 <> d2 = RouteDoc
        { rdSummary     = rdSummary d2 <|> rdSummary d1
        , rdDescription = rdDescription d2 <|> rdDescription d1
        , rdTags        = rdTags d1 ++ rdTags d2
        , rdParameters  = rdParameters d1 ++ rdParameters d2
        , rdDeprecated  = rdDeprecated d1 || rdDeprecated d2
        , rdOperationId = rdOperationId d2 <|> rdOperationId d1
        }

instance Monoid RouteDoc where
    mempty = defaultRouteDoc

-- | Recorded route info for OpenAPI generation.
data RouteInfo = RouteInfo
    { riMethod  :: Maybe StdMethod
    , riPattern :: RoutePattern
    , riDoc     :: RouteDoc
    }

--------------------- Options -----------------------
data Options = Options { verbose :: Int -- ^ 0 = silent, 1(def) = startup banner
                       , settings :: W.Settings -- ^ Warp 'Settings'
                                              -- Note: to work around an issue in warp,
                                              -- the default FD cache duration is set to 0
                                              -- so changes to static files are always picked
                                              -- up. This likely has performance implications,
                                              -- so you may want to modify this for production
                                              -- servers using `setFdCacheDuration`.
                       , jsonMode :: Bool -- ^ If True, return JSON error responses instead of HTML
                       , docsConfig :: Maybe DocsConfig -- ^ Configuration for Swagger / OpenAPI docs
                       }

defaultOptions :: Options
defaultOptions = Options 1 W.defaultSettings False (Just defaultDocsConfig)

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
                , handler :: Maybe (ErrorHandler m)
                , routeOptions :: RouteOptions
                , routeDocs :: [RouteInfo]
                , docsConfigState :: Maybe DocsConfig
                }

initScottyState :: Options -> ScottyState m
initScottyState opts = ScottyState [] [] Nothing defaultRouteOptions [] (docsConfig opts)

defaultScottyState :: ScottyState m
defaultScottyState = initScottyState defaultOptions

addRouteInfo :: RouteInfo -> ScottyState m -> ScottyState m
addRouteInfo ri s@(ScottyState {routeDocs = rds}) = s { routeDocs = ri:rds }

setDocsConfigState :: Maybe DocsConfig -> ScottyState m -> ScottyState m
setDocsConfigState mc s = s { docsConfigState = mc }

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

newtype ScottyT m a =
    ScottyT { runS :: ReaderT Options (State (ScottyState m)) a }
    deriving ( Functor, Applicative, Monad )


------------------ Scotty Errors --------------------

-- | Internal exception mechanism used to modify the request processing flow.
--
-- The exception constructor is not exposed to the user and all exceptions of this type are caught
-- and processed within the 'runAction' function.
data ActionError
  = AERedirect Status T.Text -- ^ Redirect
  | AENext -- ^ Stop processing this route and skip to the next one
  | AEFinish -- ^ Stop processing the request
  deriving (Show)
instance E.Exception ActionError

tryNext :: MonadUnliftIO m => m a -> m Bool
tryNext io = catch (io >> pure True) $
  \case
    AENext -> pure False
    _ -> pure True

-- | Specializes a 'Handler' to the 'ActionT' monad
type ErrorHandler m = Handler (ActionT m) ()

-- | Thrown e.g. when a request is too large
data ScottyException
  = RequestTooLarge
  | MalformedJSON LBS8.ByteString T.Text
  | FailedToParseJSON LBS8.ByteString T.Text
  | MalformedForm T.Text
  | PathParameterNotFound T.Text
  | QueryParameterNotFound T.Text
  | FormFieldNotFound T.Text
  | FailedToParseParameter T.Text T.Text T.Text
  | WarpRequestException W.InvalidRequest
  | WaiRequestParseException WPS.RequestParseException -- request parsing
  | ResourceTException RT.InvalidAccess -- use after free
  deriving (Show)
instance E.Exception ScottyException

------------------ Scotty Actions -------------------
type Param = (T.Text, T.Text)

-- | Type parameter @t@ is the file content. Could be @()@ when not needed or a @FilePath@ for temp files instead.
type File t = (T.Text, FileInfo t)

data ActionEnv = Env { envReq       :: Request
                     , envPathParams :: [Param]
                     , envQueryParams :: [Param]
                     , envFormDataAction :: RT.InternalState -> WPS.ParseRequestBodyOptions -> IO ([Param], [File FilePath])
                     , envBody      :: IO LBS8.ByteString
                     , envBodyChunk :: IO BS.ByteString
                     , envResponse :: TVar ScottyResponse
                     }




formParamsAndFilesWith :: MonadUnliftIO m =>
                          RT.InternalState
                       -> WPS.ParseRequestBodyOptions
                       -> ActionT m ([Param], [File FilePath])
formParamsAndFilesWith istate prbo = action `catch` (\(e :: RT.InvalidAccess) -> E.throw $ ResourceTException e)
  where
    action = do
      act <- ActionT $ asks envFormDataAction
      liftIO $ act istate prbo

getResponse :: MonadIO m => ActionEnv -> m ScottyResponse
getResponse ae = liftIO $ readTVarIO (envResponse ae)

getResponseAction :: (MonadIO m) => ActionT m ScottyResponse
getResponseAction = do
  ae <- ActionT ask
  getResponse ae

modifyResponse :: (MonadIO m) => (ScottyResponse -> ScottyResponse) -> ActionT m ()
modifyResponse f = do
  tv <- ActionT $ asks envResponse
  liftIO $ atomically $ modifyTVar' tv f

data BodyPartiallyStreamed = BodyPartiallyStreamed deriving (Show)

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


-- | The default response has code 200 OK and empty body
defaultScottyResponse :: ScottyResponse
defaultScottyResponse = SR status200 [] (ContentBuilder mempty)


newtype ActionT m a = ActionT { runAM :: ReaderT ActionEnv m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadThrow, MonadCatch, MonadBase b, MonadBaseControl b, MonadTransControl, MonadUnliftIO)

withActionEnv :: Monad m =>
                 (ActionEnv -> ActionEnv) -> ActionT m a -> ActionT m a
withActionEnv f (ActionT r) = ActionT $ local f r

instance MonadReader r m => MonadReader r (ActionT m) where
  ask = ActionT $ lift ask
  local f = ActionT . mapReaderT (local f) . runAM

-- | MonadFail instance for ActionT that converts 'fail' calls into Scotty exceptions
-- which allows these failures to be caught by Scotty's error handling system
-- and properly returned as HTTP 500 responses. The instance throws a 'StringException'
-- containing both the failure message and a call stack for debugging purposes.
instance (MonadIO m) => MonadFail (ActionT m) where
  fail msg = E.throw $ StringException msg callStack

-- | 'empty' throws 'ActionError' 'AENext', whereas '(<|>)' catches any 'ActionError's or 'StatusError's in the first action and proceeds to the second one.
instance (MonadUnliftIO m) => Alternative (ActionT m) where
  empty = E.throw AENext
  a <|> b = do
    ok <- tryAnyStatus a
    if ok then a else b
instance (MonadUnliftIO m) => MonadPlus (ActionT m) where
  mzero = empty
  mplus = (<|>)

-- | catches either ActionError (thrown by 'next'),
-- 'ScottyException' (thrown if e.g. a query parameter is not found)
tryAnyStatus :: MonadUnliftIO m => m a -> m Bool
tryAnyStatus io = (io >> pure True) `catches` [h1, h2]
  where
    h1 = Handler $ \(_ :: ActionError) -> pure False
    h2 = Handler $ \(_ :: ScottyException) -> pure False

instance (Semigroup a) => Semigroup (ScottyT m a) where
  x <> y = (<>) <$> x <*> y

instance
  ( Monoid a
  ) => Monoid (ScottyT m a) where
  mempty = return mempty

instance
  ( Monad m
  , Semigroup a
  ) => Semigroup (ActionT m a) where
  x <> y = (<>) <$> x <*> y

instance
  ( Monad m, Monoid a
  ) => Monoid (ActionT m a) where
  mempty = return mempty

------------------ Scotty Routes --------------------
data RoutePattern = Capture   T.Text
                  | Literal   T.Text
                  | Function  (Request -> Maybe [Param])

instance IsString RoutePattern where
    fromString = Capture . T.pack


