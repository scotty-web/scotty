{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.Static (static, staticRoot, staticList) where

import Control.Monad (mplus)
import Control.Monad.Trans (liftIO)
import Data.List (isInfixOf)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

import qualified Filesystem.Path.CurrentOS as F
import Network.HTTP.Types (status200)
import System.Directory (doesFileExist)

import Network.Wai

-- | Serve static files out of the application root (current directory).
-- If file is found, it is streamed to the client and no further middleware is run.
static :: Middleware
static = staticRoot ""

-- | Like 'static', but only looks for static files in the given directory.
-- Supplied path may be relative or absolute and is prepended to the requested path.
--
-- > static = staticRoot ""
staticRoot :: T.Text -> Middleware
staticRoot base app req =
    if ".." `isInfixOf` (F.encodeString fp) -- for security reasons
      then app req
      else do exists <- liftIO $ doesFileExist fStr
              if exists
                then return $ ResponseFile status200 [("Content-Type", getMimeType fp)] fStr Nothing
                else app req
  where fp = F.collapse $ F.fromText $ T.intercalate "/" $ pathInfo req
        fStr = F.encodeString $ F.fromText base F.</> fp

-- | Serve only the files given in an association list.
-- Key is the URI, Value is the filesystem path.
staticList :: [(T.Text, T.Text)] -> Middleware
staticList fs app req =
    maybe (app req)
          (\fp -> return $ ResponseFile status200 [("Content-Type", getMimeType (F.fromText fp))] (T.unpack fp) Nothing)
          ((lookup p fs) `mplus` (lookup (T.cons '/' p) fs)) -- try without and with leading slash
    where p = (T.intercalate "/" $ pathInfo req)

getMimeType :: F.FilePath -> B.ByteString
getMimeType = go . map E.encodeUtf8 . F.extensions
    where go [] = defaultMimeType
          go exts = fromMaybe (go $ tail exts) $ M.lookup (B.intercalate "." exts) defaultMimeTypes

defaultMimeType :: B.ByteString
defaultMimeType = "application/octet-stream"

-- This list taken from snap-core's Snap.Util.FileServe
defaultMimeTypes :: M.Map B.ByteString B.ByteString
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
