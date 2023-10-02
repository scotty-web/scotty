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

import UnliftIO (MonadUnliftIO(..), catch, catchAny, catches, try, tryAny, Handler(..))

catchesOptionally :: MonadUnliftIO m =>
                     m a
                  -> Maybe (Handler m a) -- ^ if present, this 'Handler' is tried first
                  -> [Handler m a] -- ^ these are tried in order
                  -> m a
catchesOptionally io mh handlers = io `catches` (maybeToList mh <> handlers)

