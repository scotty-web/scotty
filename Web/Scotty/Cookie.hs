{-|
Module      : Web.Scotty.Cookie
Copyright   : (c) 2014, 2015 Mārtiņš Mačs,
              (c) 2010, 2023 Michael Snoyman,
              (c) 2023 Marco Zocca

License     : BSD-3-Clause
Maintainer  : 
Stability   : experimental
Portability : GHC

This module provides utilities for adding cookie support inside @scotty@ applications. Most code has been adapted from 'cookie' and 'scotty-cookie'.

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
    -- * Helpers and advanced interface
    , CookiesText
    , makeSimpleCookie
    , SetCookie
    , defaultSetCookie
    , parseSetCookie
    , renderCookies
    , renderCookiesBuilder
    , renderCookiesText
    , SameSiteOption
    , sameSiteNone
    , sameSiteLax
    , sameSiteStrict
    ) where

import Control.Arrow (first, (***))
import Control.Monad ( liftM )
import Data.Char (toLower, isDigit)
import Data.Word (Word8)
import Data.Ratio (numerator, denominator)
import Data.Maybe (isJust)

-- bytestring
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.ByteString.Builder (Builder, byteString, char8, toLazyByteString)
import Data.ByteString.Builder.Extra (byteStringCopy)
import qualified Data.ByteString.Lazy as BSL (toStrict)
-- scotty
import Web.Scotty.Trans (ActionT, ScottyError(..), addHeader, header)
-- time
import Data.Time (UTCTime (UTCTime), toGregorian, fromGregorian, formatTime, parseTimeM, defaultTimeLocale)
import Data.Time.Clock (DiffTime, secondsToDiffTime)
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime )
-- text
import Data.Text (Text)
import qualified Data.Text.Lazy.Encoding as TL
import Data.Text.Encoding (encodeUtf8Builder, decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
-- deepseq
import Control.DeepSeq (NFData (rnf))




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


getCookie :: (Monad m, ScottyError e)
          => Text -- ^ name
          -> ActionT e m (Maybe Text)
getCookie c = lookup c <$> getCookies


-- | Returns all cookies
getCookies :: (Monad m, ScottyError e)
           => ActionT e m CookiesText
getCookies = liftM (maybe [] parse) $ header "Cookie"
    where parse = parseCookiesText . BSL.toStrict . TL.encodeUtf8


deleteCookie :: (Monad m, ScottyError e)
             => Text -- ^ name
             -> ActionT e m ()
deleteCookie c = setCookie $ (makeSimpleCookie c "") { setCookieExpires = Just $ posixSecondsToUTCTime 0 }


-- | Construct a simple cookie (an UTF-8 string pair with default cookie options)
makeSimpleCookie :: Text -- ^ name
                 -> Text -- ^ value
                 -> SetCookie
makeSimpleCookie n v = defaultSetCookie { setCookieName  = encodeUtf8 n
                                        , setCookieValue = encodeUtf8 v
                                        }


-- | Textual cookies. Functions assume UTF8 encoding.
type CookiesText = [(Text, Text)]

parseCookiesText :: S.ByteString -> CookiesText
parseCookiesText =
    map (go *** go) . parseCookies
  where
    go = decodeUtf8With lenientDecode

renderCookiesText :: CookiesText -> Builder
renderCookiesText = renderCookiesBuilder . map (encodeUtf8Builder *** encodeUtf8Builder)

type Cookies = [(S.ByteString, S.ByteString)]

-- | Decode the value of a \"Cookie\" request header into key/value pairs.
parseCookies :: S.ByteString -> Cookies
parseCookies s
  | S.null s = []
  | otherwise =
    let (x, y) = breakDiscard 59 s -- semicolon
     in parseCookie x : parseCookies y

parseCookie :: S.ByteString -> (S.ByteString, S.ByteString)
parseCookie s =
    let (key, value) = breakDiscard 61 s -- equals sign
        key' = S.dropWhile (== 32) key -- space
     in (key', value)

breakDiscard :: Word8 -> S.ByteString -> (S.ByteString, S.ByteString)
breakDiscard w s =
    let (x, y) = S.break (== w) s
     in (x, S.drop 1 y)

type CookieBuilder = (Builder, Builder)

renderCookiesBuilder :: [CookieBuilder] -> Builder
renderCookiesBuilder [] = mempty
renderCookiesBuilder cs =
    foldr1 go $ map renderCookie cs
  where
    go x y = x `mappend` char8 ';' `mappend` y

renderCookie :: CookieBuilder -> Builder
renderCookie (k, v) = k `mappend` char8 '=' `mappend` v

renderCookies :: Cookies -> Builder
renderCookies = renderCookiesBuilder . map (byteString *** byteString)

-- | Data type representing the key-value pair to use for a cookie, as well as configuration options for it.
--
-- ==== Creating a SetCookie
--
-- 'SetCookie' does not export a constructor; instead, use 'defaultSetCookie' and override values (see <http://www.yesodweb.com/book/settings-types> for details):
--
-- @
-- import Web.Cookie
-- :set -XOverloadedStrings
-- let cookie = 'defaultSetCookie' { 'setCookieName' = "cookieName", 'setCookieValue' = "cookieValue" }
-- @
--
-- ==== Cookie Configuration
--
-- Cookies have several configuration options; a brief summary of each option is given below. For more information, see <http://tools.ietf.org/html/rfc6265#section-4.1.2 RFC 6265> or <https://en.wikipedia.org/wiki/HTTP_cookie#Cookie_attributes Wikipedia>.
data SetCookie = SetCookie
    { setCookieName :: S.ByteString -- ^ The name of the cookie. Default value: @"name"@
    , setCookieValue :: S.ByteString -- ^ The value of the cookie. Default value: @"value"@
    , setCookiePath :: Maybe S.ByteString -- ^ The URL path for which the cookie should be sent. Default value: @Nothing@ (The browser defaults to the path of the request that sets the cookie).
    , setCookieExpires :: Maybe UTCTime -- ^ The time at which to expire the cookie. Default value: @Nothing@ (The browser will default to expiring a cookie when the browser is closed).
    , setCookieMaxAge :: Maybe DiffTime -- ^ The maximum time to keep the cookie, in seconds. Default value: @Nothing@ (The browser defaults to expiring a cookie when the browser is closed).
    , setCookieDomain :: Maybe S.ByteString -- ^ The domain for which the cookie should be sent. Default value: @Nothing@ (The browser defaults to the current domain).
    , setCookieHttpOnly :: Bool -- ^ Marks the cookie as "HTTP only", i.e. not accessible from Javascript. Default value: @False@
    , setCookieSecure :: Bool -- ^ Instructs the browser to only send the cookie over HTTPS. Default value: @False@
    , setCookieSameSite :: Maybe SameSiteOption -- ^ The "same site" policy of the cookie, i.e. whether it should be sent with cross-site requests. Default value: @Nothing@
    }
    deriving (Eq, Show)

-- | Data type representing the options for a <https://tools.ietf.org/html/draft-west-first-party-cookies-07#section-4.1 SameSite cookie>
data SameSiteOption = Lax
                    | Strict
                    | None
                    deriving (Show, Eq)

instance NFData SameSiteOption where
  rnf x = x `seq` ()

-- | Directs the browser to send the cookie for <https://tools.ietf.org/html/rfc7231#section-4.2.1 safe requests> (e.g. @GET@), but not for unsafe ones (e.g. @POST@)
sameSiteLax :: SameSiteOption
sameSiteLax = Lax

-- | Directs the browser to not send the cookie for /any/ cross-site request, including e.g. a user clicking a link in their email to open a page on your site.
sameSiteStrict :: SameSiteOption
sameSiteStrict = Strict

-- |
-- Directs the browser to send the cookie for cross-site requests.
--
-- @since 0.4.5
sameSiteNone :: SameSiteOption
sameSiteNone = None

instance NFData SetCookie where
    rnf (SetCookie a b c d e f g h i) =
        a `seq`
        b `seq`
        rnfMBS c `seq`
        rnf d `seq`
        rnf e `seq`
        rnfMBS f `seq`
        rnf g `seq`
        rnf h `seq`
        rnf i
      where
        -- For backwards compatibility
        rnfMBS Nothing = ()
        rnfMBS (Just bs) = bs `seq` ()

-- | A minimal 'SetCookie'. All fields are 'Nothing' or 'False' except @'setCookieName' = "name"@ and @'setCookieValue' = "value"@. You need this to construct a 'SetCookie', because it does not export a constructor. Equivalently, you may use 'def'.
--
-- @since 0.4.2.2
defaultSetCookie :: SetCookie
defaultSetCookie = SetCookie
    { setCookieName     = "name"
    , setCookieValue    = "value"
    , setCookiePath     = Nothing
    , setCookieExpires  = Nothing
    , setCookieMaxAge   = Nothing
    , setCookieDomain   = Nothing
    , setCookieHttpOnly = False
    , setCookieSecure   = False
    , setCookieSameSite = Nothing
    }

renderSetCookie :: SetCookie -> Builder
renderSetCookie sc = mconcat
    [ byteString (setCookieName sc)
    , char8 '='
    , byteString (setCookieValue sc)
    , case setCookiePath sc of
        Nothing -> mempty
        Just path -> byteStringCopy "; Path="
                     `mappend` byteString path
    , case setCookieExpires sc of
        Nothing -> mempty
        Just e -> byteStringCopy "; Expires=" `mappend`
                  byteString (formatCookieExpires e)
    , case setCookieMaxAge sc of
        Nothing -> mempty
        Just ma -> byteStringCopy"; Max-Age=" `mappend`
                   byteString (formatCookieMaxAge ma)
    , case setCookieDomain sc of
        Nothing -> mempty
        Just d -> byteStringCopy "; Domain=" `mappend`
                  byteString d
    , if setCookieHttpOnly sc
        then byteStringCopy "; HttpOnly"
        else mempty
    , if setCookieSecure sc
        then byteStringCopy "; Secure"
        else mempty
    , case setCookieSameSite sc of
        Nothing -> mempty
        Just Lax -> byteStringCopy "; SameSite=Lax"
        Just Strict -> byteStringCopy "; SameSite=Strict"
        Just None -> byteStringCopy "; SameSite=None"
    ]

parseSetCookie :: S.ByteString -> SetCookie
parseSetCookie a = SetCookie
    { setCookieName = name
    , setCookieValue = value
    , setCookiePath = lookup "path" flags
    , setCookieExpires =
        lookup "expires" flags >>= parseCookieExpires
    , setCookieMaxAge =
        lookup "max-age" flags >>= parseCookieMaxAge
    , setCookieDomain = lookup "domain" flags
    , setCookieHttpOnly = isJust $ lookup "httponly" flags
    , setCookieSecure = isJust $ lookup "secure" flags
    , setCookieSameSite = case lookup "samesite" flags of
        Just "Lax" -> Just Lax
        Just "Strict" -> Just Strict
        Just "None" -> Just None
        _ -> Nothing
    }
  where
    pairs = map (parsePair . dropSpace) $ S.split 59 a ++ [S8.empty] -- 59 = semicolon
    (name, value) = head pairs
    flags = map (first (S8.map toLower)) $ tail pairs
    parsePair = breakDiscard 61 -- equals sign
    dropSpace = S.dropWhile (== 32) -- space

expiresFormat :: String
expiresFormat = "%a, %d-%b-%Y %X GMT"

-- | Format a 'UTCTime' for a cookie.
formatCookieExpires :: UTCTime -> S.ByteString
formatCookieExpires =
    S8.pack . formatTime defaultTimeLocale expiresFormat

parseCookieExpires :: S.ByteString -> Maybe UTCTime
parseCookieExpires =
    fmap fuzzYear . parseTimeM True defaultTimeLocale expiresFormat . S8.unpack
  where
    -- See: https://github.com/snoyberg/cookie/issues/5
    fuzzYear orig@(UTCTime day diff)
        | x >= 70 && x <= 99 = addYear 1900
        | x >= 0 && x <= 69 = addYear 2000
        | otherwise = orig
      where
        (x, y, z) = toGregorian day
        addYear x' = UTCTime (fromGregorian (x + x') y z) diff

-- | Format a 'DiffTime' for a cookie.
formatCookieMaxAge :: DiffTime -> S.ByteString
formatCookieMaxAge difftime = S8.pack $ show (num `div` denom)
  where rational = toRational difftime
        num = numerator rational
        denom = denominator rational

parseCookieMaxAge :: S.ByteString -> Maybe DiffTime
parseCookieMaxAge bs
  | all isDigit unpacked = Just $ secondsToDiffTime $ read unpacked
  | otherwise = Nothing
  where unpacked = S8.unpack bs
