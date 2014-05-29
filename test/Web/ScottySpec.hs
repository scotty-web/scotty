{-# LANGUAGE OverloadedStrings #-}
module Web.ScottySpec (spec) where

import qualified SpecHelper            as Helper

import           Control.Applicative
import           Control.Monad
import           Data.Monoid           (mconcat)
import           Network.HTTP.Types
import           Test.Hspec
import           Web.Scotty

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

    describe "html" $ do
      it "should return response in text/html" $ do
        app <- scottyApp $
          get "/scotty" $ html "<p>scotty</p>"
        Helper.header "Content-Type" <$> app `Helper.get` "/scotty"
          `shouldReturn` Just "text/html"

      it "should return given string as html" $ do
        app <- scottyApp $
          get "/scotty" $ html "<p>scotty</p>"
        Helper.body <$> app `Helper.get` "/scotty"
          `shouldReturn` "<p>scotty</p>"
