{-# LANGUAGE OverloadedStrings #-}
-- | Miscellaneous convenience functions. If you create a helper while using
-- Scotty that you find useful, feel free to submit a patch to this file.
module Web.Scotty.Helpers
    ( addQueryString
    , paramSafe
    , paramEscaped
    ) where

import           Control.Monad

import           Data.Monoid
import qualified Data.Text.Lazy as T

import qualified Network.Wai as Wai

-- Note that we only import the monad transformer version, to force
-- us to be generic in the underyling monad. MonadIO constraints are fine.
import           Web.Scotty.Trans
import           Web.Scotty.Util

-- | Append the query string from the current request to a 'T.Text' value.
-- Useful for repassing query parameters on redirect.
--
-- > redirect =<< addQueryString "/foo"
--
addQueryString :: Monad m => T.Text -> ActionT m T.Text
addQueryString r = liftM ((r <>) . strictByteStringToLazyText . Wai.rawQueryString) request

-- * Additional functions for getting the parameters

-- | Like 'param', but returns @Maybe@ instead of doing 'raise'.
paramSafe :: (Monad m, Functor m, Parsable a) => T.Text -> ActionT m (Maybe a)
paramSafe q = fmap Just (param q)
              `rescue` \_ -> return Nothing

-- | Like 'param', but escapes the special characters and replaces
-- them with HTML entities
paramEscaped :: (Monad m, Functor m) => T.Text -> ActionT m T.Text
paramEscaped = (fmap . fmap) (T.concatMap escape) param

escape :: Char -> T.Text
escape h
    | h == '&' = "&amp;"
    | h == '\\' = "&#92;"
    | h == '"' = "&quot;"
    | h == '\'' = "&#39;"
    | h == '<' = "&lt;"
    | h == '>' = "&gt;"
    | otherwise = T.singleton h

