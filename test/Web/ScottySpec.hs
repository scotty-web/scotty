{-# LANGUAGE OverloadedStrings #-}
module Web.ScottySpec (main, spec) where

import           Test.Hspec
import           Test.Hspec.Wai
import           Network.Wai (Application)

import           Control.Monad
import           Data.Char
import           Data.String
import           Network.HTTP.Types
import qualified Control.Exception.Lifted as EL
import qualified Control.Exception as E

import           Web.Scotty as Scotty hiding (get, post, put, patch, delete, request)
import qualified Web.Scotty as Scotty

main :: IO ()
main = hspec spec

availableMethods :: [StdMethod]
availableMethods = [GET, POST, HEAD, PUT, PATCH, DELETE]

withApp :: ScottyM () -> SpecWith Application -> Spec
withApp = with . scottyApp

spec :: Spec
spec = do
  describe "ScottyM" $ do
    forM_ [
        ("GET", Scotty.get, get)
      , ("POST", Scotty.post, (`post` ""))
      , ("PUT", Scotty.put, (`put` ""))
      , ("PATCH", Scotty.patch, (`patch` ""))
      , ("DELETE", Scotty.delete, delete)
      ] $ \(method, route, makeRequest) -> do
      describe (map toLower method) $ do
        withApp (route "/scotty" $ html "") $ do
          it ("adds route for " ++ method ++ " requests") $ do
            makeRequest "/scotty" `shouldRespondWith` 200

    describe "addroute" $ do
      forM_ availableMethods $ \method -> do
        withApp (addroute method "/scotty" $ html "") $ do
          it ("can be used to add route for " ++ show method ++ " requests") $ do
            request (renderStdMethod method) "/scotty" "" `shouldRespondWith` 200

    describe "matchAny" $ do
      withApp (matchAny "/scotty" $ html "") $ do
        forM_ availableMethods $ \method -> do
          it ("adds route that matches " ++ show method ++ " requests") $ do
            request (renderStdMethod method) "/scotty" "" `shouldRespondWith` 200

    describe "notFound" $ do
      withApp (notFound $ html "my custom not found page") $ do
        it "adds handler for requests that do not match any route" $ do
          get "/somewhere" `shouldRespondWith` "my custom not found page" {matchStatus = 404}

  describe "ActionM" $ do
    withApp (Scotty.get "/" $ (undefined `EL.catch` ((\_ -> html "") :: E.SomeException -> ActionM ()))) $ do
      it "has a MonadBaseControl instance" $ do
        get "/" `shouldRespondWith` 200

    describe "param" $ do
      withApp (Scotty.get "/search" $ param "query" >>= text) $ do
        it "returns query parameter with given name" $ do
          get "/search?query=haskell" `shouldRespondWith` "haskell"

    describe "text" $ do
      let modernGreekText :: IsString a => a
          modernGreekText = "νέα ελληνικά"

      withApp (Scotty.get "/scotty" $ text modernGreekText) $ do
        it "sets body to given text" $ do
          get "/scotty" `shouldRespondWith` modernGreekText

        it "sets Content-Type header to \"text/plain; charset=utf-8\"" $ do
          get "/scotty" `shouldRespondWith` 200 {matchHeaders = [("Content-Type", "text/plain; charset=utf-8")]}

    describe "html" $ do
      let russianLanguageTextInHtml :: IsString a => a
          russianLanguageTextInHtml = "<p>ру́сский язы́к</p>"

      withApp (Scotty.get "/scotty" $ html russianLanguageTextInHtml) $ do
        it "sets body to given text" $ do
          get "/scotty" `shouldRespondWith` russianLanguageTextInHtml

        it "sets Content-Type header to \"text/html; charset=utf-8\"" $ do
          get "/scotty" `shouldRespondWith` 200 {matchHeaders = [("Content-Type", "text/html; charset=utf-8")]}
