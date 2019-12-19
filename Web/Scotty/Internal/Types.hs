{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Web.Scotty.Internal.Types where

import           Blaze.ByteString.Builder (Builder)

import           Control.Applicative
import qualified Control.Exception as E
import           Control.Monad.Base (MonadBase, liftBase, liftBaseDefault)
import           Control.Monad.Catch (MonadCatch, catch, MonadThrow, throwM)
import           Control.Monad.Error.Class
import qualified Control.Monad.Fail as Fail
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Control (MonadBaseControl, StM, liftBaseWith, restoreM, ComposeSt, defaultLiftBaseWith, defaultRestoreM, MonadTransControl, StT, liftWith, restoreT)
import           Control.Monad.Trans.Except

import qualified Data.ByteString as BS
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Default.Class (Default, def)
import           Data.String (IsString(..))
import           Data.Text.Lazy (Text, pack)
import           Data.Typeable (Typeable)

import           Network.HTTP.Types

import           Network.Wai hiding (Middleware, Application)
import qualified Network.Wai as Wai
import           Network.Wai.Handler.Warp (Settings, defaultSettings)
import           Network.Wai.Parse (FileInfo)

import           Prelude ()
import           Prelude.Compat

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

instance Default (ScottyState e m) where
    def = ScottyState [] [] Nothing

addMiddleware :: Wai.Middleware -> ScottyState e m -> ScottyState e m
addMiddleware m s@(ScottyState {middlewares = ms}) = s { middlewares = m:ms }

addRoute :: Middleware m -> ScottyState e m -> ScottyState e m
addRoute r s@(ScottyState {routes = rs}) = s { routes = r:rs }

addHandler :: ErrorHandler e m -> ScottyState e m -> ScottyState e m
addHandler h s = s { handler = h }

newtype ScottyT e m a = ScottyT { runS :: State (ScottyState e m) a }
    deriving ( Functor, Applicative, Monad )


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
    showError (Redirect url)  = url
    showError Next            = pack "Next"
    showError Finish          = pack "Finish"
    showError (ActionError _ e) = showError e

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

newtype ActionT e m a = ActionT { runAM :: ExceptT (ActionError e) (ReaderT ActionEnv (StateT ScottyResponse m)) a }
    deriving ( Functor, Applicative, MonadIO )

instance (Monad m, ScottyError e) => Monad (ActionT e m) where
    return = ActionT . return
    ActionT m >>= k = ActionT (m >>= runAM . k)
#if !(MIN_VERSION_base(4,13,0))
    fail = Fail.fail
#endif

instance (Monad m, ScottyError e) => Fail.MonadFail (ActionT e m) where
    fail = ActionT . throwError . stringError

instance ( Monad m, ScottyError e
#if !(MIN_VERSION_base(4,8,0))
         , Functor m
#endif
         ) => Alternative (ActionT e m) where
    empty = mzero
    (<|>) = mplus

instance (Monad m, ScottyError e) => MonadPlus (ActionT e m) where
    mzero = ActionT . ExceptT . return $ Left Next
    ActionT m `mplus` ActionT n = ActionT . ExceptT $ do
        a <- runExceptT m
        case a of
            Left  _ -> runExceptT n
            Right r -> return $ Right r

instance MonadTrans (ActionT e) where
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

instance MonadTransControl (ActionT e) where
     type StT (ActionT e) a = StT (StateT ScottyResponse) (StT (ReaderT ActionEnv) (StT (ExceptT (ActionError e)) a))
     liftWith = \f ->
        ActionT $  liftWith $ \run  ->
                   liftWith $ \run' ->
                   liftWith $ \run'' ->
                   f $ run'' . run' . run . runAM
     restoreT = ActionT . restoreT . restoreT . restoreT

instance (ScottyError e, MonadBaseControl b m) => MonadBaseControl b (ActionT e m) where
    type StM (ActionT e m) a = ComposeSt (ActionT e) m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM

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

instance
  ( Monoid a
#if !(MIN_VERSION_base(4,11,0))
  , Semigroup a
#endif
#if !(MIN_VERSION_base(4,8,0))
  , Functor m
#endif
  ) => Monoid (ScottyT e m a) where
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
  ) => Semigroup (ActionT e m a) where
  x <> y = (<>) <$> x <*> y

instance
  ( Monad m, ScottyError e, Monoid a
#if !(MIN_VERSION_base(4,11,0))
  , Semigroup a
#endif
#if !(MIN_VERSION_base(4,8,0))
  , Functor m
#endif
  ) => Monoid (ActionT e m a) where
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
