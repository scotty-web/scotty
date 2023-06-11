{-|
Module      : Web.Scotty.Cookie
Copyright   : (c) 2014, 2015 Mārtiņš Mačs,
              (c) 2023 Marco Zocca

License     : BSD-3-Clause
Maintainer  :
Stability   : experimental
Portability : GHC

This module provides utilities for adding cookie support inside @scotty@ applications. Most code has been adapted from 'scotty-cookie'.

== Example

A simple hit counter that stores the number of page visits in a cookie:

@
\{\-\# LANGUAGE OverloadedStrings \#\-\}

import Control.Monad
import Data.Monoid
import Data.Maybe
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Read as TL (decimal)
import Web.Scotty (scotty, html)
import Web.Scotty.Cookie (getCookie, setSimpleCookie)

main :: IO ()
main = scotty 3000 $
    get \"/\" $ do
        hits <- liftM (fromMaybe \"0\") $ 'getCookie' \"hits\"
        let hits' =
              case TL.decimal hits of
                Right n -> TL.pack . show . (+1) $ (fst n :: Integer)
                Left _  -> \"1\"
        'setSimpleCookie' \"hits\" $ TL.toStrict hits'
        html $ mconcat [ \"\<html\>\<body\>\"
                       , hits'
                       , \"\<\/body\>\<\/html\>\"
                       ]
@
-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Scotty.Cookie (
    -- * Set cookie
      setCookie
    , setSimpleCookie
    -- * Get cookie(s)
    , getCookie
    , getCookies
    -- * Delete a cookie
    , deleteCookie
    -- * Helpers and advanced interface (re-exported from 'cookie')
    , CookiesText
    , makeSimpleCookie
    -- ** cookie configuration
    , SetCookie
    , setCookieName
    , setCookieValue
    , setCookiePath
    , setCookieExpires
    , setCookieMaxAge
    , setCookieDomain
    , setCookieHttpOnly
    , setCookieSecure
    , setCookieSameSite
    , SameSiteOption
    , sameSiteNone
    , sameSiteLax
    , sameSiteStrict
    , defaultSetCookie
    -- ** parsing and rendering
    , parseSetCookie
    , renderCookies
    , renderCookiesText
    ) where

-- bytestring
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as BSL (toStrict)
-- cookie
import Web.Cookie (SetCookie, setCookieName , setCookieValue, setCookiePath, setCookieExpires, setCookieMaxAge, setCookieDomain, setCookieHttpOnly, setCookieSecure, setCookieSameSite, parseSetCookie, renderCookies, renderCookiesText, renderSetCookie, defaultSetCookie, CookiesText, parseCookiesText, SameSiteOption, sameSiteStrict, sameSiteNone, sameSiteLax)
-- scotty
import Web.Scotty.Trans (ActionT, ScottyError(..), addHeader, header)
-- time
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime )
-- text
import Data.Text (Text)
import qualified Data.Text.Encoding as T (encodeUtf8)
import qualified Data.Text.Lazy.Encoding as TL (encodeUtf8, decodeUtf8)



-- | Set a cookie, with full access to its options (see 'SetCookie')
setCookie :: (Monad m, ScottyError e)
          => SetCookie
          -> ActionT e m ()
setCookie c = addHeader "Set-Cookie" (TL.decodeUtf8 . toLazyByteString $ renderSetCookie c)


-- | 'makeSimpleCookie' and 'setCookie' combined.
setSimpleCookie :: (Monad m, ScottyError e)
                => Text -- ^ name
                -> Text -- ^ value
                -> ActionT e m ()
setSimpleCookie n v = setCookie $ makeSimpleCookie n v

-- | Lookup one cookie name
getCookie :: (Monad m, ScottyError e)
          => Text -- ^ name
          -> ActionT e m (Maybe Text)
getCookie c = lookup c <$> getCookies


-- | Returns all cookies
getCookies :: (Monad m, ScottyError e)
           => ActionT e m CookiesText
getCookies = (maybe [] parse) <$> header "Cookie"
    where parse = parseCookiesText . BSL.toStrict . TL.encodeUtf8

-- | Browsers don't directly delete a cookie, but setting its expiry to a past date (e.g. the UNIX epoch) ensures that the cookie will be invalidated (whether and when it will be actually deleted by the browser seems to be browser-dependent).
deleteCookie :: (Monad m, ScottyError e)
             => Text -- ^ name
             -> ActionT e m ()
deleteCookie c = setCookie $ (makeSimpleCookie c "") { setCookieExpires = Just $ posixSecondsToUTCTime 0 }


-- | Construct a simple cookie (an UTF-8 string pair with default cookie options)
makeSimpleCookie :: Text -- ^ name
                 -> Text -- ^ value
                 -> SetCookie
makeSimpleCookie n v = defaultSetCookie { setCookieName  = T.encodeUtf8 n
                                        , setCookieValue = T.encodeUtf8 v
                                        }

