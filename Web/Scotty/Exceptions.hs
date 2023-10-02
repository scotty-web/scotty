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

import Data.Maybe (maybeToList)

import UnliftIO (MonadUnliftIO(..), catch, catchAny, catches, try, tryAny, Handler(..))


-- | Handlers are tried sequentially
catchesOptionally :: MonadUnliftIO m =>
                     m a
                  -> Maybe (Handler m a) -- ^ if present, this 'Handler' is tried first
                  -> [Handler m a] -- ^ these are tried in order
                  -> m a
catchesOptionally io mh handlers = io `catches` (maybeToList mh <> handlers)
