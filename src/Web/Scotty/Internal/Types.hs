{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Scotty.Internal.Types where

import Control.Applicative
import Control.Monad.Error.Class
import Control.Monad.Trans.Except
import Data.Monoid
import Network.HTTP.Types
import Prelude.Compat

import Blaze.ByteString.Builder (Builder)
import Control.Exception (Exception)
import Control.Monad (MonadPlus (..))
import Control.Monad.Base (MonadBase, liftBase, liftBaseDefault)
import Control.Monad.Catch (MonadCatch, MonadThrow, catch, throwM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader (..), ReaderT, mapReaderT)
import Control.Monad.State.Strict (MonadState (..), State, StateT, mapStateT)
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Default.Class (Default, def)
import Data.String (IsString (..))
import Data.Text.Lazy (Text, pack)
import Data.Typeable (Typeable)
import Network.Wai hiding (Application, Middleware)
import Network.Wai.Handler.Warp (Settings, defaultSettings)
import Network.Wai.Parse (FileInfo)
import Prelude ()

import qualified Control.Exception as E
import qualified Control.Monad.Fail as Fail
import qualified Control.Monad.Trans.Control as MTC
import qualified Data.ByteString as BS
import qualified Network.Wai as Wai

--------------------- Options -----------------------
data Options = Options
    { verbose :: Int
    -- ^ 0 = silent, 1(def) = startup banner
    , settings :: Settings
    -- ^ Warp 'Settings'
    -- Note: to work around an issue in warp,
    -- the default FD cache duration is set to 0
    -- so changes to static files are always picked
    -- up. This likely has performance implications,
    -- so you may want to modify this for production
    -- servers using `setFdCacheDuration`.
    }

instance Default Options where
    def = Options 1 defaultSettings

newtype RouteOptions = RouteOptions
    { maxRequestBodySize :: Maybe Kilobytes -- max allowed request size in KB
    }

instance Default RouteOptions where
    def = RouteOptions Nothing

type Kilobytes = Int

----- Transformer Aware Applications/Middleware -----
type Middleware m = Application m -> Application m
type Application m = Request -> m Response

--------------- Scotty Applications -----------------
data ScottyState e m = ScottyState
    { middlewares :: [Wai.Middleware]
    , routes :: [Middleware m]
    , handler :: ErrorHandler e m
    , routeOptions :: RouteOptions
    }

instance Default (ScottyState e m) where
    def = ScottyState [] [] Nothing def

addMiddleware :: Wai.Middleware -> ScottyState e m -> ScottyState e m
addMiddleware m s@(ScottyState{middlewares = ms}) = s{middlewares = m : ms}

addRoute :: Middleware m -> ScottyState e m -> ScottyState e m
addRoute r s@(ScottyState{routes = rs}) = s{routes = r : rs}

addHandler :: ErrorHandler e m -> ScottyState e m -> ScottyState e m
addHandler h s = s{handler = h}

updateMaxRequestBodySize :: RouteOptions -> ScottyState e m -> ScottyState e m
updateMaxRequestBodySize RouteOptions{..} s@ScottyState{routeOptions = ro} =
    let ro' = ro{maxRequestBodySize = maxRequestBodySize}
    in  s{routeOptions = ro'}

newtype ScottyT e m a = ScottyT {runS :: State (ScottyState e m) a}
    deriving (Functor, Applicative, Monad)

------------------ Scotty Errors --------------------
data ActionError e
    = Redirect Text
    | Next
    | Finish
    | ActionError Status e

-- | In order to use a custom exception type (aside from 'Text'), you must
-- define an instance of 'ScottyError' for that type.
class ScottyError e where
    stringError :: String -> e
    showError :: e -> Text

instance ScottyError Text where
    stringError = pack
    showError = id

instance ScottyError e => ScottyError (ActionError e) where
    stringError = ActionError status500 . stringError
    showError (Redirect url) = url
    showError Next = pack "Next"
    showError Finish = pack "Finish"
    showError (ActionError _ e) = showError e

type ErrorHandler e m = Maybe (e -> ActionT e m ())

data ScottyException = RequestException BS.ByteString Status deriving (Show, Typeable)

instance Exception ScottyException

------------------ Scotty Actions -------------------
type Param = (Text, Text)

type File = (Text, FileInfo ByteString)

data ActionEnv = Env
    { getReq :: Request
    , getParams :: [Param]
    , getBody :: IO ByteString
    , getBodyChunk :: IO BS.ByteString
    , getFiles :: [File]
    }

data RequestBodyState
    = BodyUntouched
    | BodyCached ByteString [BS.ByteString] -- whole body, chunks left to stream
    | BodyCorrupted

data BodyPartiallyStreamed = BodyPartiallyStreamed deriving (Show, Typeable)

instance E.Exception BodyPartiallyStreamed

data Content
    = ContentBuilder Builder
    | ContentFile FilePath
    | ContentStream StreamingBody

data ScottyResponse = SR
    { srStatus :: Status
    , srHeaders :: ResponseHeaders
    , srContent :: Content
    }

instance Default ScottyResponse where
    def = SR status200 [] (ContentBuilder mempty)

newtype ActionT e m a = ActionT {runAM :: ExceptT (ActionError e) (ReaderT ActionEnv (StateT ScottyResponse m)) a}
    deriving newtype (Functor, Applicative, Monad, MonadIO)
    deriving (Semigroup, Monoid) via (Ap (ActionT e m) a)

instance (Monad m, ScottyError e) => Fail.MonadFail (ActionT e m) where
    fail = ActionT . throwError . stringError

instance (Monad m, ScottyError e) => Alternative (ActionT e m) where
    empty = ActionT . ExceptT . return $ Left Next
    ActionT m <|> ActionT n = ActionT . ExceptT $ do
        a <- runExceptT m
        case a of
            Left _ -> runExceptT n
            Right r -> return $ Right r

instance (Monad m, ScottyError e) => MonadPlus (ActionT e m)

instance ScottyError e => MonadTrans (ActionT e) where
    lift = ActionT . lift . lift . lift

instance (ScottyError e, Monad m) => MonadError (ActionError e) (ActionT e m) where
    throwError = ActionT . throwError

    catchError (ActionT m) f = ActionT (catchError m (runAM . f))

instance (MonadBase b m, ScottyError e) => MonadBase b (ActionT e m) where
    liftBase = liftBaseDefault

instance (MonadThrow m, ScottyError e) => MonadThrow (ActionT e m) where
    throwM = ActionT . throwM

instance (MonadCatch m, ScottyError e) => MonadCatch (ActionT e m) where
    catch (ActionT m) f = ActionT (m `catch` (runAM . f))

instance ScottyError e => MTC.MonadTransControl (ActionT e) where
    type StT (ActionT e) a = MTC.StT (StateT ScottyResponse) (MTC.StT (ReaderT ActionEnv) (MTC.StT (ExceptT (ActionError e)) a))
    liftWith f =
        ActionT $ MTC.liftWith $ \run ->
            MTC.liftWith $ \run' ->
                MTC.liftWith $ \run'' ->
                    f $ run'' . run' . run . runAM
    restoreT = ActionT . MTC.restoreT . MTC.restoreT . MTC.restoreT

instance (ScottyError e, MTC.MonadBaseControl b m) => MTC.MonadBaseControl b (ActionT e m) where
    type StM (ActionT e m) a = MTC.ComposeSt (ActionT e) m a
    liftBaseWith = MTC.defaultLiftBaseWith
    restoreM = MTC.defaultRestoreM

instance (MonadReader r m, ScottyError e) => MonadReader r (ActionT e m) where
    {-# INLINE ask #-}
    ask = lift ask
    {-# INLINE local #-}
    local f = ActionT . mapExceptT (mapReaderT (mapStateT $ local f)) . runAM

instance (MonadState s m, ScottyError e) => MonadState s (ActionT e m) where
    {-# INLINE get #-}
    get = lift get
    {-# INLINE put #-}
    put = lift . put

instance (Semigroup a) => Semigroup (ScottyT e m a) where
    x <> y = (<>) <$> x <*> y

instance (Monoid a) => Monoid (ScottyT e m a) where
    mempty = return mempty

------------------ Scotty Routes --------------------
data RoutePattern
    = Capture Text
    | Literal Text
    | Function (Request -> Maybe [Param])

instance IsString RoutePattern where
    fromString = Capture . pack
