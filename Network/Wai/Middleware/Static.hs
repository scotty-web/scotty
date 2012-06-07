{-# LANGUAGE OverloadedStrings #-}
-- | Serve static files, subject to a policy that can filter or
--   modify incoming URIs. The flow is:
--
--   incoming request URI ==> policies ==> exists? ==> respond
--
--   If any of the polices fail (return Nothing), or the file doesn't
--   exist, then the middleware gives up and calls the inner application.
--   If the file is found, the middleware chooses a content type based
--   on the file extension and returns the file contents as the response.
module Network.Wai.Middleware.Static
    ( -- * Middlewares
      static, staticPolicy
    , -- * Policies
      Policy, (>->)
    , addBase, addSlash, noDots, only
    ) where

import Control.Monad.Trans (liftIO)
import Data.List (isInfixOf)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid
import qualified Data.Text as T

import Network.HTTP.Types (status200, Ascii)
import System.Directory (doesFileExist)
import System.FilePath

import Network.Wai

-- | Take an incoming URI and optionally modify or filter it.
--   The result will be treated as a filepath.
type Policy = String -> Maybe String

-- | Combine two policies. They are run from left to right.
(>->) :: Policy -> Policy -> Policy
p1 >-> p2 = maybe Nothing p2 . p1

-- | Filter URIs containing \"..\"
noDots :: Policy
noDots s = if ".." `isInfixOf` s then Nothing else Just s

-- | Add a base path to the URI
--
-- > staticPolicy (addBase "/home/user/files")
--
-- GET \"foo\/bar\" looks for \"\/home\/user\/files\/foo\/bar\"
--
addBase :: String -> Policy
addBase b = Just . (b </>)

-- | Add an initial slash to to the URI, if not already present.
--
-- > staticPolicy addSlash
--
-- GET \"foo\/bar\" looks for \"\/foo\/bar\"
addSlash :: Policy
addSlash s@('/':_) = Just s
addSlash s         = Just ('/':s)

-- | Filter any URIs not in a specific list, mapping to a filepath.
--
-- > staticPolicy (only [("foo/bar", "/home/user/files/bar")])
--
-- GET \"foo\/bar\" looks for \"\/home\/user\/files\/bar\"
-- GET \"baz\/bar\" doesn't match anything
--
only :: [(String,String)] -> Policy
only = flip lookup

-- | Serve static files out of the application root (current directory).
-- If file is found, it is streamed to the client and no further middleware is run.
static :: Middleware
static = staticPolicy mempty

-- | Serve static files subject to a 'Policy'
staticPolicy :: Policy -> Middleware
staticPolicy p app req =
    maybe (app req)
          (\fp -> do exists <- liftIO $ doesFileExist fp
                     if exists
                        then return $ ResponseFile status200
                                                   [("Content-Type", getMimeType fp)]
                                                   fp
                                                   Nothing
                        else app req)
          (p $ T.unpack $ T.intercalate "/" $ pathInfo req)

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
