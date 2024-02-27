-- | This should be in 'hspec-wai', PR pending as of Feb 2024 : https://github.com/hspec/hspec-wai/pull/77 

{-# language OverloadedStrings #-}
module Test.Hspec.Wai.Extra (postMultipartForm, FileMeta(..)) where

import qualified Data.Char as Char
import Data.List (intersperse)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LB

import Data.Word (Word8)

import Network.HTTP.Types (methodPost, hContentType)
import Network.Wai.Test (SResponse)

import Test.Hspec.Wai (request)
import Test.Hspec.Wai.Internal (WaiSession)

-- | @POST@ a @multipart/form-data@ form which might include files.
--
-- The @Content-Type@ is set to @multipart/form-data; boundary=<bd>@ where @bd@ is the part separator without the @--@ prefix.
postMultipartForm :: ByteString -- ^ path
                  -> ByteString -- ^ part separator without any dashes
                  -> [(FileMeta, ByteString, ByteString, ByteString)] -- ^ (file metadata, field MIME type, field name, field contents)
                  -> WaiSession st SResponse
postMultipartForm path sbs =
  request methodPost path [(hContentType, "multipart/form-data; boundary=" <> sbs)] . formMultipartQuery sbs

-- | Encode the body of a multipart form post
--
-- schema from : https://swagger.io/docs/specification/describing-request-body/multipart-requests/
formMultipartQuery :: ByteString -- ^ part separator without any dashes
                   -> [(FileMeta, ByteString, ByteString, ByteString)] -- ^ (file metadata, field MIME type, field name, field contents)
                   -> LB.ByteString
formMultipartQuery sbs = Builder.toLazyByteString . mconcat . (preamble :) . intersperse newline . encodeAll
  where
    preamble =
      kv "Content-Type" ("multipart/form-data; boundary=" <> Builder.byteString sbs <> newline <> newline)
    encodeAll fs = map encodeFile fs <> [sepEnd]
    encodeFile (fieldMeta, ty, n, payload) = mconcat $ [
      sep
      , newline
      , kv "Content-Disposition" ("form-data;" <> " name=" <> quoted n <> encodeMPField fieldMeta)
      , newline
      , kv "Content-Type" (Builder.byteString ty)
      , newline, newline
      , Builder.byteString payload
      ]
    sep = Builder.byteString ("--" <> sbs)
    sepEnd = Builder.byteString ("--" <> sbs <> "--")
    encodeMPField FMFormField = mempty
    encodeMPField (FMFile fname) = "; filename=" <> quoted fname
    quoted x = Builder.byteString ("\"" <> x <> "\"")
    kv k v = k <> ": " <> v
    newline = Builder.word8 (ord '\n')


data FileMeta = FMFormField -- ^ any form field except a file
              | FMFile ByteString -- ^ file name


ord :: Char -> Word8
ord = fromIntegral . Char.ord
