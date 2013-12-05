-- | Miscellaneous convenience functions. If you create a helper while using
-- Scotty that you find useful, feel free to submit a patch to this file.
module Web.Scotty.Helpers
    ( addQueryString
    ) where

import           Control.Monad

import           Data.Monoid
import qualified Data.Text.Lazy as T

import qualified Network.Wai as Wai

-- Note that we only import the monad transformer version, to force
-- us to be generic in the underyling monad. MonadIO constraints are fine.
import           Web.Scotty.Types
import           Web.Scotty.Trans
import           Web.Scotty.Util

-- | Append the query string from the current request to a 'T.Text' value.
-- Useful for repassing query parameters on redirect.
--
-- > redirect =<< addQueryString "/foo"
--
addQueryString :: (ScottyError e, Monad m) => T.Text -> ActionT e m T.Text
addQueryString r = liftM ((r <>) . strictByteStringToLazyText . Wai.rawQueryString) request
