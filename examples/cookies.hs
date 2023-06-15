{-# LANGUAGE OverloadedStrings #-}
-- This examples requires you to: cabal install blaze-html
module Main (main) where

import Control.Monad (forM_)

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty
import Web.Scotty.Cookie (CookiesText, setSimpleCookie, getCookies)

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
            renderCookiesTable cookies
            H.form H.! method "post" H.! action "/set-a-cookie" $ do
                H.input H.! type_ "text" H.! name "name"
                H.input H.! type_ "text" H.! name "value"
                H.input H.! type_ "submit" H.! value "set a cookie"

    post "/set-a-cookie" $ do
        name'  <- param "name"
        value' <- param "value"
        setSimpleCookie name' value'
        redirect "/"
