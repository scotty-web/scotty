{-# LANGUAGE ExistentialQuantification #-}
module Web.Scotty.Exceptions (
  Handler(..)
  -- * catching
  , catch
  , catchAny
  , catches
  , catchesOptionally
  -- * trying
  , try
  , tryAny
                             ) where

import Control.Exception (Exception (..), SomeException (..), SomeAsyncException (..))
import qualified Control.Exception as EUnsafe (fromException, throwIO, catch)
import           Control.Monad.IO.Class (MonadIO(..))
import Data.Maybe (maybeToList)

import Control.Monad.IO.Unlift (MonadUnliftIO(..))

-- | Handler for a specific type of exception, see 'handleActionError'
data Handler m a = forall e . Exception e => Handler (e -> m a)
instance Monad m => Functor (Handler m) where
  fmap f (Handler h) = Handler (\e -> f <$> h e)


-- exceptions

catchesOptionally :: MonadUnliftIO m =>
                     m a
                  -> Maybe (Handler m a) -- ^ if present, this 'Handler' is tried first
                  -> [Handler m a] -- ^ these are tried in order
                  -> m a
catchesOptionally io mh handlers = io `catches` (maybeToList mh <> handlers)

catches :: MonadUnliftIO m => m a -> [Handler m a] -> m a
catches io handlers = io `catch` catchesHandler handlers

catchesHandler :: MonadIO m => [Handler m a] -> SomeException -> m a
catchesHandler handlers e = foldr tryHandler (liftIO (EUnsafe.throwIO e)) handlers
    where tryHandler (Handler h) res
              = case EUnsafe.fromException e of
                Just e' -> h e'
                Nothing -> res

-- | (from 'unliftio') Catch a synchronous (but not asynchronous) exception and recover from it.
catch
  :: (MonadUnliftIO m, Exception e)
  => m a -- ^ action
  -> (e -> m a) -- ^ handler
  -> m a
catch f g = withRunInIO $ \run -> run f `EUnsafe.catch` \e ->
  if isSyncException e
    then run (g e)
    -- intentionally rethrowing an async exception synchronously,
    -- since we want to preserve async behavior
    else EUnsafe.throwIO e

-- | 'catch' specialized to catch all synchronous exceptions.
catchAny :: MonadUnliftIO m => m a -> (SomeException -> m a) -> m a
catchAny = catch

-- | (from 'safe-exceptions') Check if the given exception is synchronous
isSyncException :: Exception e => e -> Bool
isSyncException e =
    case EUnsafe.fromException (toException e) of
        Just (SomeAsyncException _) -> False
        Nothing -> True

try :: (MonadUnliftIO m, Exception e) => m a -> m (Either e a)
try f = catch (Right <$> f) (pure . Left)

-- | 'try' specialized to catch all synchronous exceptions.
tryAny :: MonadUnliftIO m => m a -> m (Either SomeException a)
tryAny = try
