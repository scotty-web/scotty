module Web.Scotty.Trans.Lazy where

import Control.Monad (join)
import Control.Monad.IO.Class
import Data.Bifunctor (bimap)
import qualified Data.Text.Lazy as T
  
import Network.HTTP.Types (Status)

import qualified Web.Scotty.Action as Base
import Web.Scotty.Internal.Types

-- | Throw a "500 Server Error" 'StatusError', which can be caught with 'rescue'.
--
-- Uncaught exceptions turn into HTTP 500 responses.
raise :: (MonadIO m) =>
         T.Text -- ^ Error text
      -> ActionT m a
raise  = Base.raise . T.toStrict
{-# DEPRECATED raise "Throw an exception instead" #-}

-- | Throw a 'StatusError' exception that has an associated HTTP error code and can be caught with 'rescue'.
--
-- Uncaught exceptions turn into HTTP responses corresponding to the given status.
raiseStatus :: Monad m => Status -> T.Text -> ActionT m a
raiseStatus s = Base.raiseStatus s . T.toStrict
{-# DEPRECATED raiseStatus "Use status, text, and finish instead" #-}

-- | Redirect to given URL. Like throwing an uncatchable exception. Any code after the call to redirect
-- will not be run.
--
-- > redirect "http://www.google.com"
--
-- OR
--
-- > redirect "/foo/bar"
redirect :: (Monad m) => T.Text -> ActionT m a
redirect = Base.redirect . T.toStrict

-- | Get a request header. Header name is case-insensitive.
header :: (Monad m) => T.Text -> ActionT m (Maybe T.Text)
header h = fmap T.fromStrict <$> Base.header (T.toStrict h)

-- | Get all the request headers. Header names are case-insensitive.
headers :: (Monad m) => ActionT m [(T.Text, T.Text)]
headers = map (join bimap T.fromStrict) <$> Base.headers

-- | Add to the response headers. Header names are case-insensitive.
addHeader :: MonadIO m => T.Text -> T.Text -> ActionT m ()
addHeader k v = Base.addHeader (T.toStrict k) (T.toStrict v)

-- | Set one of the response headers. Will override any previously set value for that header.
-- Header names are case-insensitive.
setHeader :: MonadIO m => T.Text -> T.Text -> ActionT m ()
setHeader k v = Base.addHeader (T.toStrict k) (T.toStrict v)


-- | Add to the response headers. Header names are case-insensitive.
addHeader1 :: MonadIO m => T.Text -> T.Text -> ActionT m ()
addHeader1 k v = Base.addHeader1 (T.toStrict k) (T.toStrict v)

-- | Set one of the response headers. Will override any previously set value for that header.
-- Header names are case-insensitive.
setHeader1 :: MonadIO m => T.Text -> T.Text -> ActionT m ()
setHeader1 k v = Base.addHeader1 (T.toStrict k) (T.toStrict v)


text :: (MonadIO m) => T.Text -> ActionT m ()
text = Base.textLazy

html :: (MonadIO m) => T.Text -> ActionT m ()
html = Base.htmlLazy
