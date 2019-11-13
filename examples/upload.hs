{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Web.Scotty

import Control.Monad.IO.Class

import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Parse

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Text (renderHtml)

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BS
import System.FilePath ((</>))
import Prelude ()
import Prelude.Compat

main :: IO ()
main = scotty 3000 $ do
    middleware logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "uploads")

    get "/" $ do
        html $ renderHtml
             $ H.html $ do
                H.body $ do
                    H.form H.! method "post" H.! enctype "multipart/form-data" H.! action "/upload" $ do
                        H.input H.! type_ "file" H.! name "foofile"
                        H.br
                        H.input H.! type_ "file" H.! name "barfile"
                        H.br
                        H.input H.! type_ "submit"

    post "/upload" $ do
        fs <- files
        let fs' = [ (fieldName, BS.unpack (fileName fi), fileContent fi) | (fieldName,fi) <- fs ]
        -- write the files to disk, so they will be served by the static middleware
        liftIO $ sequence_ [ B.writeFile ("uploads" </> fn) fc | (_,fn,fc) <- fs' ]
        -- generate list of links to the files just uploaded
        html $ mconcat [ mconcat [ fName
                                 , ": "
                                 , renderHtml $ H.a (H.toHtml fn) H.! (href $ H.toValue fn) >> H.br
                                 ]
                       | (fName,fn,_) <- fs' ]
