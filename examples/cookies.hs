{-# LANGUAGE OverloadedStrings #-}
-- This examples requires you to: cabal install cookie
-- and: cabal install blaze-html
module Main (main) where

import Control.Monad (forM_)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Blaze.ByteString.Builder as B

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty
import Web.Cookie

makeCookie :: BS.ByteString -> BS.ByteString -> SetCookie
makeCookie n v = def { setCookieName = n, setCookieValue = v }

renderSetCookie' :: SetCookie -> Text
renderSetCookie' = T.decodeUtf8 . B.toLazyByteString . renderSetCookie

setCookie :: BS.ByteString -> BS.ByteString -> ActionM ()
setCookie n v = setHeader "Set-Cookie" (renderSetCookie' (makeCookie n v))

getCookies :: ActionM (Maybe CookiesText)
getCookies =
    fmap (fmap (parseCookiesText . lazyToStrict . T.encodeUtf8)) $
        header "Cookie"
    where
        lazyToStrict = BS.concat . BSL.toChunks

renderCookiesTable :: CookiesText -> H.Html
renderCookiesTable cs =
    H.table $ do
        H.tr $ do
            H.th "name"
            H.th "value"
        forM_ cs $ \(name', val) -> do
            H.tr $ do
                H.td (H.toMarkup name')
                H.td (H.toMarkup val)

main :: IO ()
main = scotty 3000 $ do
    get "/" $ do
        cookies <- getCookies
        html $ renderHtml $ do
            case cookies of
                Just cs -> renderCookiesTable cs
                Nothing -> return ()
            H.form H.! method "post" H.! action "/set-a-cookie" $ do
                H.input H.! type_ "text" H.! name "name"
                H.input H.! type_ "text" H.! name "value"
                H.input H.! type_ "submit" H.! value "set a cookie"

    post "/set-a-cookie" $ do
        name'  <- param "name"
        value' <- param "value"
        setCookie name' value'
        redirect "/"
