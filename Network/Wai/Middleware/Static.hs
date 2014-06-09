{-# LANGUAGE OverloadedStrings #-}
-- | Serve static files, subject to a policy that can filter or
--   modify incoming URIs. The flow is:
--
--   incoming request URI ==> policies ==> exists? ==> respond
--
--   If any of the polices fail, or the file doesn't
--   exist, then the middleware gives up and calls the inner application.
--   If the file is found, the middleware chooses a content type based
--   on the file extension and returns the file contents as the response.
module Network.Wai.Middleware.Static
    ( -- * Middlewares
      static, staticPolicy, unsafeStaticPolicy
    , -- * Policies
      Policy, (<|>), (>->), policy, predicate
    , addBase, addSlash, contains, hasPrefix, hasSuffix, noDots, isNotAbsolute, only
    , -- * Utilities
      tryPolicy
    ) where

import Control.Monad.Trans (liftIO)
import qualified Data.ByteString as B
import Data.List
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid
import qualified Data.Text as T

import Network.HTTP.Types (status200)
import System.Directory (doesFileExist)
import qualified System.FilePath as FP

import Network.Wai

-- | Take an incoming URI and optionally modify or filter it.
--   The result will be treated as a filepath.
newtype Policy = Policy { tryPolicy :: String -> Maybe String -- ^ Run a policy
                        }

-- | Note:
--   'mempty' == @policy Just@ (the always accepting policy)
--   'mappend' == @>->@ (policy sequencing)
instance Monoid Policy where
    mempty = policy Just
    mappend p1 p2 = policy (maybe Nothing (tryPolicy p2) . tryPolicy p1)

-- | Lift a function into a 'Policy'
policy :: (String -> Maybe String) -> Policy
policy = Policy

-- | Lift a predicate into a 'Policy'
predicate :: (String -> Bool) -> Policy
predicate p = policy (\s -> if p s then Just s else Nothing)

-- | Sequence two policies. They are run from left to right. (Note: this is `mappend`)
infixr 5 >->
(>->) :: Policy -> Policy -> Policy
(>->) = mappend

-- | Choose between two policies. If the first fails, run the second.
infixr 4 <|>
(<|>) :: Policy -> Policy -> Policy
p1 <|> p2 = policy (\s -> maybe (tryPolicy p2 s) Just (tryPolicy p1 s))

-- | Add a base path to the URI
--
-- > staticPolicy (addBase "/home/user/files")
--
-- GET \"foo\/bar\" looks for \"\/home\/user\/files\/foo\/bar\"
--
addBase :: String -> Policy
addBase b = policy (Just . (b FP.</>))

-- | Add an initial slash to to the URI, if not already present.
--
-- > staticPolicy addSlash
--
-- GET \"foo\/bar\" looks for \"\/foo\/bar\"
addSlash :: Policy
addSlash = policy slashOpt
    where slashOpt s@('/':_) = Just s
          slashOpt s         = Just ('/':s)

-- | Accept only URIs with given suffix
hasSuffix :: String -> Policy
hasSuffix = predicate . isSuffixOf

-- | Accept only URIs with given prefix
hasPrefix :: String -> Policy
hasPrefix = predicate . isPrefixOf

-- | Accept only URIs containing given string
contains :: String -> Policy
contains = predicate . isInfixOf

-- | Reject URIs containing \"..\"
noDots :: Policy
noDots = predicate (not . isInfixOf "..")

-- | Reject URIs that are absolute paths
isNotAbsolute :: Policy
isNotAbsolute = predicate $ not . FP.isAbsolute

-- | Use URI as the key to an association list, rejecting those not found.
-- The policy result is the matching value.
--
-- > staticPolicy (only [("foo/bar", "/home/user/files/bar")])
--
-- GET \"foo\/bar\" looks for \"\/home\/user\/files\/bar\"
-- GET \"baz\/bar\" doesn't match anything
--
only :: [(String,String)] -> Policy
only al = policy (flip lookup al)

-- | Serve static files out of the application root (current directory).
-- If file is found, it is streamed to the client and no further middleware is run.
--
-- Note: for security reasons, this uses the 'noDots' and 'isNotAbsolute' policy by default.
static :: Middleware
static = staticPolicy mempty

-- | Serve static files subject to a 'Policy'
--
-- Note: for security reasons, this uses the 'noDots' and 'isNotAbsolute' policy by default.
staticPolicy :: Policy -> Middleware
staticPolicy p = unsafeStaticPolicy $ noDots >-> isNotAbsolute >-> p

-- | Serve static files subject to a 'Policy'. Unlike 'static' and 'staticPolicy', this
-- has no policies enabled by default, and is hence insecure.
unsafeStaticPolicy :: Policy -> Middleware
unsafeStaticPolicy p app req callback =
    maybe (app req callback)
          (\fp -> do exists <- liftIO $ doesFileExist fp
                     if exists
                        then callback $ responseFile status200
                                                     [("Content-Type", getMimeType fp)]
                                                     fp
                                                     Nothing
                        else app req callback)
          (tryPolicy p $ T.unpack $ T.intercalate "/" $ pathInfo req)

type Ascii = B.ByteString

getMimeType :: FilePath -> Ascii
getMimeType = go . extensions
    where go [] = defaultMimeType
          go (ext:exts) = fromMaybe (go exts) $ M.lookup ext defaultMimeTypes

extensions :: FilePath -> [String]
extensions [] = []
extensions fp = case dropWhile (/= '.') fp of
                    [] -> []
                    s -> let ext = tail s
                         in ext : extensions ext

defaultMimeType :: Ascii
defaultMimeType = "application/octet-stream"

-- This list taken from snap-core's Snap.Util.FileServe
defaultMimeTypes :: M.Map String Ascii
defaultMimeTypes = M.fromList [
  ( "asc"     , "text/plain"                        ),
  ( "asf"     , "video/x-ms-asf"                    ),
  ( "asx"     , "video/x-ms-asf"                    ),
  ( "avi"     , "video/x-msvideo"                   ),
  ( "bz2"     , "application/x-bzip"                ),
  ( "c"       , "text/plain"                        ),
  ( "class"   , "application/octet-stream"          ),
  ( "conf"    , "text/plain"                        ),
  ( "cpp"     , "text/plain"                        ),
  ( "css"     , "text/css"                          ),
  ( "cxx"     , "text/plain"                        ),
  ( "dtd"     , "text/xml"                          ),
  ( "dvi"     , "application/x-dvi"                 ),
  ( "gif"     , "image/gif"                         ),
  ( "gz"      , "application/x-gzip"                ),
  ( "hs"      , "text/plain"                        ),
  ( "htm"     , "text/html"                         ),
  ( "html"    , "text/html"                         ),
  ( "jar"     , "application/x-java-archive"        ),
  ( "jpeg"    , "image/jpeg"                        ),
  ( "jpg"     , "image/jpeg"                        ),
  ( "js"      , "text/javascript"                   ),
  ( "json"    , "application/json"                  ),
  ( "log"     , "text/plain"                        ),
  ( "m3u"     , "audio/x-mpegurl"                   ),
  ( "mov"     , "video/quicktime"                   ),
  ( "mp3"     , "audio/mpeg"                        ),
  ( "mpeg"    , "video/mpeg"                        ),
  ( "mpg"     , "video/mpeg"                        ),
  ( "ogg"     , "application/ogg"                   ),
  ( "pac"     , "application/x-ns-proxy-autoconfig" ),
  ( "pdf"     , "application/pdf"                   ),
  ( "png"     , "image/png"                         ),
  ( "ps"      , "application/postscript"            ),
  ( "qt"      , "video/quicktime"                   ),
  ( "sig"     , "application/pgp-signature"         ),
  ( "spl"     , "application/futuresplash"          ),
  ( "svg"     , "image/svg+xml"                     ),
  ( "swf"     , "application/x-shockwave-flash"     ),
  ( "tar"     , "application/x-tar"                 ),
  ( "tar.bz2" , "application/x-bzip-compressed-tar" ),
  ( "tar.gz"  , "application/x-tgz"                 ),
  ( "tbz"     , "application/x-bzip-compressed-tar" ),
  ( "text"    , "text/plain"                        ),
  ( "tgz"     , "application/x-tgz"                 ),
  ( "torrent" , "application/x-bittorrent"          ),
  ( "ttf"     , "application/x-font-truetype"       ),
  ( "txt"     , "text/plain"                        ),
  ( "wav"     , "audio/x-wav"                       ),
  ( "wax"     , "audio/x-ms-wax"                    ),
  ( "wma"     , "audio/x-ms-wma"                    ),
  ( "wmv"     , "video/x-ms-wmv"                    ),
  ( "xbm"     , "image/x-xbitmap"                   ),
  ( "xml"     , "text/xml"                          ),
  ( "xpm"     , "image/x-xpixmap"                   ),
  ( "xwd"     , "image/x-xwindowdump"               ),
  ( "zip"     , "application/zip"                   ) ]
