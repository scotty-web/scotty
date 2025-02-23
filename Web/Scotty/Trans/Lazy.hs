module Web.Scotty.Trans.Lazy where

import Control.Monad (join)
import Control.Monad.IO.Class
import Data.Bifunctor (bimap)
import qualified Data.Text.Lazy as T
  
import qualified Web.Scotty.Action as Base
import Web.Scotty.Internal.Types

-- | Synonym for 'redirect302'.
-- If you are unsure which redirect to use, you probably want this one.
--
-- > redirect "http://www.google.com"
--
-- OR
--
-- > redirect "/foo/bar"
redirect :: (Monad m) => T.Text -> ActionT m a
redirect = redirect302

-- | Redirect to given URL with status 300 (Multiple Choices). Like throwing
-- an uncatchable exception. Any code after the call to
-- redirect will not be run.
redirect300 :: (Monad m) => T.Text -> ActionT m a
redirect300 = Base.redirect300 . T.toStrict

-- | Redirect to given URL with status 301 (Moved Permanently). Like throwing
-- an uncatchable exception. Any code after the call to
-- redirect will not be run.
redirect301 :: (Monad m) => T.Text -> ActionT m a
redirect301 = Base.redirect301 . T.toStrict

-- | Redirect to given URL with status 302 (Found). Like throwing
-- an uncatchable exception. Any code after the call to
-- redirect will not be run.
redirect302 :: (Monad m) => T.Text -> ActionT m a
redirect302 = Base.redirect302 . T.toStrict

-- | Redirect to given URL with status 303 (See Other). Like throwing
-- an uncatchable exception. Any code after the call to
-- redirect will not be run.
redirect303 :: (Monad m) => T.Text -> ActionT m a
redirect303 = Base.redirect303 . T.toStrict

-- | Redirect to given URL with status 304 (Not Modified). Like throwing
-- an uncatchable exception. Any code after the call to
-- redirect will not be run.
redirect304 :: (Monad m) => T.Text -> ActionT m a
redirect304 = Base.redirect304 . T.toStrict

-- | Redirect to given URL with status 307 (Temporary Redirect). Like throwing
-- an uncatchable exception. Any code after the call to
-- redirect will not be run.
redirect307 :: (Monad m) => T.Text -> ActionT m a
redirect307 = Base.redirect307 . T.toStrict

-- | Redirect to given URL with status 308 (Permanent Redirect). Like throwing
-- an uncatchable exception. Any code after the call to
-- redirect will not be run.
redirect308 :: (Monad m) => T.Text -> ActionT m a
redirect308 = Base.redirect308 . T.toStrict

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

text :: (MonadIO m) => T.Text -> ActionT m ()
text = Base.textLazy

html :: (MonadIO m) => T.Text -> ActionT m ()
html = Base.htmlLazy
