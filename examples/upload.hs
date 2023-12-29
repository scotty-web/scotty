{-# LANGUAGE OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
module Main (main) where

import Web.Scotty

import Control.Exception (SomeException)
import Control.Monad.IO.Class
import Data.Foldable (for_)
import qualified Data.Text.Lazy as TL

import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Parse (fileName, fileContent)

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Text (renderHtml)

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BS
import System.FilePath ((</>))

{-| NB : the file paths where files are saved and looked up are relative, so make sure
to run this program from the root directory of the 'scotty' repo, or adjust the paths
accordingly.
-}

main :: IO ()
main = scotty 3000 $ do
    middleware logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "examples/uploads")

    get "/" $ do
        html $ renderHtml
             $ H.html $ do
                H.body $ do
                    H.form H.! method "post" H.! enctype "multipart/form-data" H.! action "/upload" $ do
                        H.input H.! type_ "file" H.! name "file_1"
                        H.br
                        H.input H.! type_ "file" H.! name "file_2"
                        H.br
                        H.input H.! type_ "submit"

    post "/upload" $ do
      filesOpts defaultParseRequestBodyOptions $ \_ fs -> do
        let
          fs' = [(fieldName, BS.unpack (fileName fi), fileContent fi) | (fieldName, fi) <- fs]
        -- write the files to disk, so they can be served by the static middleware
        for_ fs' $ \(_, fn, fpath) -> do
          -- copy temp file to local dir
          liftIO (do
                     fc <- B.readFile fpath
                     B.writeFile ("examples" </> "uploads" </> fn) fc
                 ) `catch` (\(e :: SomeException) -> do
                               liftIO $ putStrLn $ unwords ["upload: something went wrong while saving temp files :", show e]
                           )
          -- generate list of links to the files just uploaded
          html $ mconcat [ mconcat [ TL.fromStrict fName
                                   , ": "
                                   , renderHtml $ H.a (H.toHtml fn) H.! (href $ H.toValue fn) >> H.br
                                   ]
                         | (fName,fn,_) <- fs' ]
