{-# LANGUAGE OverloadedStrings #-}
module Web.ScottySpec (spec) where

import qualified SpecHelper            as Helper

import           Control.Applicative
import           Control.Monad
import           Data.Monoid             (mconcat)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Network.HTTP.Types
import           Test.Hspec
import           Web.Scotty
import qualified Control.Exception.Lifted as EL
import qualified Control.Exception as E

spec :: Spec
spec =  do
    let availableMethods = [GET, POST, HEAD, PUT, DELETE, PATCH]

    describe "get" $
      it "should route GET request" $ do
        app <- scottyApp $ get "/scotty" $ html ""
        Helper.status <$> app `Helper.get` "/scotty" `shouldReturn` status200

    describe "post" $
      it "should route POST request" $ do
        app <- scottyApp $ post "/scotty" $ html ""
        Helper.status <$> Helper.post app "/scotty" "" `shouldReturn` status200

    describe "put" $
      it "should route PUT request" $ do
        app <- scottyApp $ put "/scotty" $ html ""
        Helper.status <$> Helper.put app "/scotty" "" `shouldReturn` status200

    describe "delete" $
      it "should route DELETE request" $ do
        app <- scottyApp $ delete "/scotty" $ html ""
        Helper.status <$> Helper.delete app "/scotty" `shouldReturn` status200

    describe "patch" $
      it "should route PATCH request" $ do
        app <- scottyApp $ patch "/scotty" $ html ""
        Helper.status <$> Helper.patch app "/scotty" "" `shouldReturn` status200

    describe "addroute" $
      it ("should route " ++ show availableMethods ++ " request") $
        forM_ availableMethods $ \(method) -> do
          app <- scottyApp $
            addroute method "/scotty" $ html ""
          Helper.status <$> Helper.request app (renderStdMethod method) "/scotty" ""
            `shouldReturn` status200

    describe "matchAny" $
      it ("should route " ++ show availableMethods ++ " request") $
        forM_ availableMethods $ \(method) -> do
          app <- scottyApp $
            matchAny "/scotty" $ html ""
          Helper.status <$> Helper.request app (renderStdMethod method) "/scotty" ""
            `shouldReturn` status200

    describe "notFound" $
      it "should route all request" $ do
        app <- scottyApp $
          notFound $ html "routed to not found"
        Helper.body <$> Helper.get app "/somewhere"
          `shouldReturn` "routed to not found"

    describe "param" $
      it "should return query parameter with given key" $ do
        app <- scottyApp $
          get "/search" $ do
            query <- param "query"
            html $ mconcat ["<p>", query, "</p>"]
        Helper.body <$> app `Helper.get` "/search?query=haskell"
          `shouldReturn` "<p>haskell</p>"

    describe "text" $ do
      let
        modernGreekText = "νέα ελληνικά"
        app' = scottyApp $ get "/scotty" $ text modernGreekText

      it "should return response in text/plain encoded in utf-8" $ do
        app <- app'
        Helper.header "Content-Type" <$> app `Helper.get` "/scotty"
          `shouldReturn` Just "text/plain; charset=utf-8"

      it "should return given string as text" $ do
        app <- app'
        Helper.body <$> app `Helper.get` "/scotty"
          `shouldReturn` (encodeUtf8 modernGreekText)

    describe "html" $ do
      let
        russianLanguageTextInHtml = "<p>ру́сский язы́к</p>"
        app' = scottyApp $ get "/scotty" $ html russianLanguageTextInHtml

      it "should return response in text/html encoded in utf-8" $ do
        app <- app'
        Helper.header "Content-Type" <$> app `Helper.get` "/scotty"
          `shouldReturn` Just "text/html; charset=utf-8"

      it "should return given string as html" $ do
        app <- app'
        Helper.body <$> app `Helper.get` "/scotty"
          `shouldReturn` (encodeUtf8 russianLanguageTextInHtml)

    describe "lifted-base" $
      it "should not return the default exception handler" $ do
        app <- scottyApp $ get "/" $ ((undefined) `EL.catch` ((\_ -> html "") :: E.SomeException -> ActionM ()))
        Helper.status <$> app `Helper.get` "/" `shouldReturn` status200
