{-# LANGUAGE OverloadedStrings, CPP, ScopedTypeVariables, DeriveGeneric #-}
module Web.ScottySpec (main, spec) where

import           Test.Hspec
import           Test.Hspec.Wai (WaiSession, with, request, get, post, put, patch, delete, options, (<:>), shouldRespondWith, matchHeaders, matchBody, matchStatus)
import Test.Hspec.Wai.Extra (postMultipartForm, FileMeta(..))

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.String
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time (UTCTime(..))
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (secondsToDiffTime)

import GHC.Generics (Generic)

import           Network.HTTP.Types
import           Network.Wai (Application, Request(queryString), responseLBS)
import           Network.Wai.Middleware.ValidateHeaders (validateHeadersMiddleware, defaultValidateHeadersSettings)
import           Network.Wai.Parse (defaultParseRequestBodyOptions)
import           Network.Wai.Test (SResponse)
import qualified Control.Exception.Lifted as EL
import qualified Control.Exception as E

import           Web.FormUrlEncoded (FromForm)
import           Web.Scotty as Scotty hiding (get, post, put, patch, delete, request, options)
import qualified Web.Scotty as Scotty
import qualified Web.Scotty.Cookie as SC (getCookie, setSimpleCookie, deleteCookie)

#if !defined(mingw32_HOST_OS)
import           Control.Concurrent.Async (withAsync)
import           Control.Exception (bracketOnError)
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import           Network.Socket (Family(..), SockAddr(..), Socket, SocketOption(..), SocketType(..), bind, close, connect, listen, maxListenQueue, setSocketOption, socket)
import           Network.Socket.ByteString (send, recv)
import           System.Directory (removeFile)
#endif

main :: IO ()
main = hspec spec

availableMethods :: [StdMethod]
availableMethods = [GET, POST, HEAD, PUT, PATCH, DELETE, OPTIONS]

data SearchForm = SearchForm
  { sfQuery :: Text
  , sfYear :: Int
  } deriving (Generic)

instance FromForm SearchForm where

postForm :: ByteString -> LBS.ByteString -> WaiSession st SResponse
postForm p = request "POST" p [("Content-Type","application/x-www-form-urlencoded")]

spec :: Spec
spec = do
  let withApp = with . scottyApp
  describe "ScottyM" $ do
    forM_ [
        ("GET", Scotty.get, get)
      , ("POST", Scotty.post, (`post` ""))
      , ("PUT", Scotty.put, (`put` ""))
      , ("PATCH", Scotty.patch, (`patch` ""))
      , ("DELETE", Scotty.delete, delete)
      , ("OPTIONS", Scotty.options, options)
      ] $ \(method, route, makeRequest) -> do
      describe (map toLower method) $ do
        withApp (route "/scotty" $ html "") $ do
          it ("adds route for " ++ method ++ " requests") $ do
            makeRequest "/scotty" `shouldRespondWith` 200

        withApp (route "/scotty" $ html "") $ do
          it ("properly handles extra slash routes for " ++ method ++ " requests") $ do
            makeRequest "//scotty" `shouldRespondWith` 200

        withApp (route "/:paramName" $ captureParam "paramName" >>= text) $ do
          it ("captures route parameters for " ++ method ++ " requests when parameter matches its name") $ do
            makeRequest "/:paramName" `shouldRespondWith` ":paramName"
          it ("captures route parameters for " ++ method ++ " requests with url encoded '/' in path") $ do
            makeRequest "/a%2Fb" `shouldRespondWith` "a/b"

    describe "addroute" $ do
      forM_ availableMethods $ \method -> do
        withApp (addroute method "/scotty" $ html "") $ do
          it ("can be used to add route for " ++ show method ++ " requests") $ do
            request (renderStdMethod method) "/scotty" [] "" `shouldRespondWith` 200

    describe "matchAny" $ do
      withApp (matchAny "/scotty" $ html "") $ do
        forM_ ("NONSTANDARD" : fmap renderStdMethod availableMethods) $ \method -> do
          it ("adds route that matches " ++ show method ++ " requests") $ do
            request method "/scotty" [] "" `shouldRespondWith` 200

    describe "notFound" $ do
      withApp (notFound $ html "my custom not found page") $ do
        it "adds handler for requests that do not match any route" $ do
          get "/somewhere" `shouldRespondWith` "my custom not found page" {matchStatus = 404}

      withApp (notFound $ status status400 >> html "my custom not found page") $ do
        it "allows to customize the HTTP status code" $ do
          get "/somewhere" `shouldRespondWith` "my custom not found page" {matchStatus = 400}

      context "when not specified" $ do
        withApp (return ()) $ do
          it "returns 404 when no route matches" $ do
            get "/" `shouldRespondWith` "<h1>404: File Not Found!</h1>" {matchStatus = 404}

    describe "defaultHandler" $ do
      withApp (do
                  let h = Handler (\(e :: E.ArithException) -> status status500 >> text (TL.pack $ show e))
                  defaultHandler h
                  Scotty.get "/" (throw E.DivideByZero)) $ do
        it "sets custom exception handler" $ do
          get "/" `shouldRespondWith` "divide by zero" {matchStatus = 500}
      withApp (do
                  let h = Handler (\(_ :: E.ArithException) -> status status503)
                  defaultHandler h
                  Scotty.get "/" (liftIO $ E.throwIO E.DivideByZero)) $ do
        it "allows to customize the HTTP status code" $ do
          get "/" `shouldRespondWith` "" {matchStatus = 503}
      withApp (do
                  let h = Handler (\(_ :: E.SomeException) -> setHeader "Location" "/c" >> status status500)
                  defaultHandler h
                  Scotty.get "/a" (redirect "/b")) $ do
        it "should give priority to actionErrorHandlers" $ do
          get "/a" `shouldRespondWith` 302 { matchHeaders = ["Location" <:> "/b"] }

      context "when not specified" $ do
        withApp (Scotty.get "/" $ throw E.DivideByZero) $ do
          it "returns 500 on exceptions" $ do
            get "/" `shouldRespondWith` 500
      context "only applies to endpoints defined after it (#237)" $ do
        withApp (do
                    let h = Handler (\(_ :: E.SomeException) -> status status503 >> text "ok")
                    Scotty.get "/a" (throw E.DivideByZero)
                    defaultHandler h
                    Scotty.get "/b" (throw E.DivideByZero)
                      ) $ do
          it "doesn't catch an exception before the handler is set" $ do
            get "/a" `shouldRespondWith` 500
          it "catches an exception after the handler is set" $ do
            get "/b" `shouldRespondWith` "ok" {matchStatus = 503}


    describe "setMaxRequestBodySize" $ do
      let
        large = TLE.encodeUtf8 . TL.pack . concat $ [show c | c <- ([1..4500]::[Integer])]
        smol = TLE.encodeUtf8 . TL.pack . concat $ [show c | c <- ([1..50]::[Integer])]
      withApp (do
                  Scotty.setMaxRequestBodySize 1
                  Scotty.post "/upload" $ do
                    _ <- files
                    status status200
              ) $ do
        context "application/x-www-form-urlencoded" $ do
          it "should return 200 OK if the request body size is below 1 KB" $ do
            request "POST" "/upload" [("Content-Type","application/x-www-form-urlencoded")]
              smol `shouldRespondWith` 200
          it "should return 413 (Content Too Large) if the request body size is above 1 KB" $ do
            request "POST" "/upload" [("Content-Type","application/x-www-form-urlencoded")]
              large `shouldRespondWith` 413

      withApp (Scotty.post "/" $ status status200) $ do
          context "(counterexample)" $ do
            it "doesn't throw an uncaught exception if the body is large" $ do
              request "POST" "/" [("Content-Type","application/x-www-form-urlencoded")]
                large `shouldRespondWith` 200
      withApp (Scotty.setMaxRequestBodySize 1 >> Scotty.post "/upload" (do status status200)) $ do
        context "multipart/form-data; boundary=--33" $ do
          it "should return 200 OK if the request body size is above 1 KB (since multipart form bodies are only traversed or parsed on demand)" $ do
            request "POST" "/upload" [("Content-Type","multipart/form-data; boundary=--33")]
              large `shouldRespondWith` 200

    describe "middleware" $ do
      context "can rewrite the query string (#348)" $ do
        withApp (do
                    Scotty.middleware $ \app req sendResponse ->
                      app req{queryString = [("query", Just "haskell")]} sendResponse
                    Scotty.matchAny "/search" $ queryParam "query" >>= text
                ) $ do
         it "returns query parameter with given name" $ do
           get "/search" `shouldRespondWith` "haskell"

      context "ValidateHeaders middleware" $ do
        context "rejects invalid header values" $ do
          withApp (do
                      Scotty.middleware $ validateHeadersMiddleware defaultValidateHeadersSettings
                      Scotty.get "/test" $ do
                        setHeader "X-Custom" "value\r\nwith\r\nnewlines"
                        text "should not reach here"
                  ) $ do
            it "returns 500 when header value contains CR/LF" $ do
              get "/test" `shouldRespondWith` 500

          withApp (do
                      Scotty.middleware $ validateHeadersMiddleware defaultValidateHeadersSettings
                      Scotty.get "/test" $ do
                        setHeader "X-Custom" "value\nwith\nnewlines"
                        text "should not reach here"
                  ) $ do
            it "returns 500 when header value contains LF" $ do
              get "/test" `shouldRespondWith` 500

          withApp (do
                      Scotty.middleware $ validateHeadersMiddleware defaultValidateHeadersSettings
                      Scotty.get "/test" $ do
                        setHeader "X-Custom" "value\0with\0null"
                        text "should not reach here"
                  ) $ do
            it "returns 500 when header value contains NUL" $ do
              get "/test" `shouldRespondWith` 500

          withApp (do
                      Scotty.middleware $ validateHeadersMiddleware defaultValidateHeadersSettings
                      Scotty.get "/test" $ do
                        setHeader "X-Custom" "trailing space "
                        text "should not reach here"
                  ) $ do
            it "returns 500 when header value has trailing whitespace" $ do
              get "/test" `shouldRespondWith` 500

          withApp (do
                      Scotty.middleware $ validateHeadersMiddleware defaultValidateHeadersSettings
                      Scotty.get "/test" $ do
                        setHeader "X-Custom" " leading space"
                        text "should not reach here"
                  ) $ do
            it "returns 500 when header value has leading whitespace" $ do
              get "/test" `shouldRespondWith` 500

        context "allows valid header values" $ do
          withApp (do
                      Scotty.middleware $ validateHeadersMiddleware defaultValidateHeadersSettings
                      Scotty.get "/test" $ do
                        setHeader "X-Custom" "valid-value"
                        text "success"
                  ) $ do
            it "returns 200 when header value is valid" $ do
              get "/test" `shouldRespondWith` "success" {matchStatus = 200}

          withApp (do
                      Scotty.middleware $ validateHeadersMiddleware defaultValidateHeadersSettings
                      Scotty.get "/test" $ do
                        setHeader "X-Custom" "value with spaces"
                        text "success"
                  ) $ do
            it "returns 200 when header value contains internal spaces" $ do
              get "/test" `shouldRespondWith` "success" {matchStatus = 200}

          withApp (do
                      Scotty.middleware $ validateHeadersMiddleware defaultValidateHeadersSettings
                      Scotty.get "/test" $ do
                        setHeader "Content-Type" "text/plain; charset=utf-8"
                        text "success"
                  ) $ do
            it "returns 200 for standard Content-Type header" $ do
              get "/test" `shouldRespondWith` 200

        context "can be disabled by not adding the middleware" $ do
          withApp (do
                      Scotty.get "/test" $ do
                        setHeader "X-Custom" "value\r\nwith\r\nnewlines"
                        text "no validation"
                  ) $ do
            it "allows invalid headers when middleware is not enabled" $ do
              get "/test" `shouldRespondWith` "no validation" {matchStatus = 200}

  describe "ActionM" $ do
    context "MonadBaseControl instance" $ do
        withApp (Scotty.get "/" $ (undefined `EL.catch` ((\_ -> html "") :: E.SomeException -> ActionM ()))) $ do
          it "catches SomeException and returns 200" $ do
            get "/" `shouldRespondWith` 200
        withApp (Scotty.get "/" $ EL.throwIO E.DivideByZero) $ do
          it "returns 500 on uncaught exceptions" $ do
            get "/" `shouldRespondWith` 500

    context "Alternative instance" $ do
      withApp (Scotty.get "/" $ empty >>= text) $
        it "empty without any route following returns a 404" $
          get "/" `shouldRespondWith` 404
      withApp (Scotty.get "/dictionary" $ empty <|> queryParam "word1" >>= text) $
        it "empty throws Next" $ do
          get "/dictionary?word1=x" `shouldRespondWith` "x"
      withApp (Scotty.get "/dictionary" $ queryParam "word1" <|> empty <|> queryParam "word2" >>= text) $
        it "<|> skips the left route if that fails" $ do
          get "/dictionary?word2=y" `shouldRespondWith` "y"
          get "/dictionary?word1=a&word2=b" `shouldRespondWith` "a"

    context "MonadFail instance" $ do
      withApp (Scotty.get "/" $ fail "boom!") $ do
        it "returns 500 if not caught" $
          get "/" `shouldRespondWith` 500
      withApp (Scotty.get "/" $ fail "boom!" `catch` (\(_ :: E.SomeException) -> do 
        status status200
        text "ok")) $
        it "can catch the Exception thrown by fail" $ do
          get "/" `shouldRespondWith` 200 { matchBody = "ok"}

    describe "redirect" $ do
      withApp (
        do
          Scotty.get "/a" $ redirect "/b"
              ) $ do
        it "Responds with a 302 Redirect" $ do
          get "/a" `shouldRespondWith` 302 { matchHeaders = ["Location" <:> "/b"] }

    describe "redirect300" $ do
      withApp (
        do
          Scotty.get "/a" $ redirect300 "/b"
              ) $ do
        it "Responds with a 300 Redirect" $ do
          get "/a" `shouldRespondWith` 300 { matchHeaders = ["Location" <:> "/b"] }


    describe "redirect301" $ do
      withApp (
        do
          Scotty.get "/a" $ redirect301 "/b"
              ) $ do
        it "Responds with a 301 Redirect" $ do
          get "/a" `shouldRespondWith` 301 { matchHeaders = ["Location" <:> "/b"] }

    describe "redirect302" $ do
      withApp (
        do
          Scotty.get "/a" $ redirect302 "/b"
              ) $ do
        it "Responds with a 302 Redirect" $ do
          get "/a" `shouldRespondWith` 302 { matchHeaders = ["Location" <:> "/b"] }


    describe "redirect303" $ do
      withApp (
        do
          Scotty.delete "/a" $ redirect303 "/b"
              ) $ do
        it "Responds with a 303 as passed in" $ do
          delete "/a" `shouldRespondWith` 303 { matchHeaders = ["Location" <:> "/b"]}

    describe "redirect304" $ do
      withApp (
        do
          Scotty.get "/a" $ redirect304 "/b"
              ) $ do
        it "Responds with a 304 Redirect" $ do
          get "/a" `shouldRespondWith` 304 { matchHeaders = ["Location" <:> "/b"] }

    describe "redirect307" $ do
      withApp (
        do
          Scotty.get "/a" $ redirect307 "/b"
              ) $ do
        it "Responds with a 307 Redirect" $ do
          get "/a" `shouldRespondWith` 307 { matchHeaders = ["Location" <:> "/b"] }

    describe "redirect308" $ do
      withApp (
        do
          Scotty.get "/a" $ redirect308 "/b"
              ) $ do
        it "Responds with a 308 Redirect" $ do
          get "/a" `shouldRespondWith` 308 { matchHeaders = ["Location" <:> "/b"] }

    describe "Parsable" $ do
      it "parses a UTCTime string" $ do
        parseParam "2023-12-18T00:38:00Z" `shouldBe` Right (UTCTime (fromGregorian 2023 12 18) (secondsToDiffTime (60 * 38)) )

    describe "captureParam" $ do
      withApp (
        do
          Scotty.get "/search/:q" $ do
            _ :: Int <- captureParam "q"
            text "int"
          Scotty.get "/search/:q" $ do
            _ :: String <- captureParam "q"
            text "string"
          Scotty.get "/search-time/:q" $ do
            t :: UTCTime <- captureParam "q"
            text $ TL.pack (show t)
              ) $ do
        it "responds with 200 OK iff at least one route matches at the right type" $ do
          get "/search/42" `shouldRespondWith` 200 { matchBody = "int" }
          get "/search/potato" `shouldRespondWith` 200 { matchBody = "string" }
          get "/search-time/2023-12-18T00:38:00Z" `shouldRespondWith` 200 {matchBody = "2023-12-18 00:38:00 UTC"}
      withApp (
        do
          Scotty.get "/search/:q" $ do
            v <- captureParam "q"
            json (v :: Int)
              ) $ do
        it "responds with 404 Not Found if no route matches at the right type" $ do
          get "/search/potato" `shouldRespondWith` 404
      withApp (
        do
          Scotty.matchAny "/search/:q" $ do
            v <- captureParam "zzz"
            json (v :: Int)
              ) $ do
        it "responds with 500 Server Error if the parameter cannot be found in the capture" $ do
          get "/search/potato" `shouldRespondWith` 500
      context "recover from missing parameter exception" $ do
        withApp (Scotty.get "/search/:q" $
                 (captureParam "z" >>= text) `catch` (\(_::ScottyException) -> text "z")
                ) $ do
          it "catches a StatusError" $ do
            get "/search/xxx" `shouldRespondWith` 200 { matchBody = "z"}

    describe "queryParam" $ do
      withApp (Scotty.get "/search" $ queryParam "query" >>= text) $ do
        it "returns query parameter with given name" $ do
          get "/search?query=haskell" `shouldRespondWith` "haskell"
        it "decodes URL-encoding" $ do
          get "/search?query=Kurf%C3%BCrstendamm" `shouldRespondWith` "Kurfürstendamm"
      withApp (Scotty.matchAny "/search" (do
                                             v <- queryParam "query"
                                             json (v :: Int) )) $ do
        it "responds with 200 OK if the query parameter can be parsed at the right type" $ do
          get "/search?query=42" `shouldRespondWith` 200
        it "responds with 400 Bad Request if the query parameter cannot be parsed at the right type" $ do
          get "/search?query=potato" `shouldRespondWith` 400
      context "recover from type mismatch parameter exception" $ do
        withApp (Scotty.get "/search" $
                 (queryParam "z" >>= (\v -> json (v :: Int))) `catch` (\(_::ScottyException) -> text "z")
                ) $ do
          it "catches a ScottyException" $ do
            get "/search?query=potato" `shouldRespondWith` 200 { matchBody = "z"}
    
    describe "formData" $ do
      withApp (Scotty.post "/search" $ formData >>= (text . sfQuery)) $ do
        it "decodes the form" $ do
          postForm "/search" "sfQuery=Haskell&sfYear=2024" `shouldRespondWith` "Haskell"

        it "decodes URL-encoding" $ do
          postForm "/search" "sfQuery=Kurf%C3%BCrstendamm&sfYear=2024" `shouldRespondWith` "Kurfürstendamm"

        it "returns 400 when the form is malformed" $ do
          postForm "/search" "sfQuery=Haskell" `shouldRespondWith` 400

    describe "formParam" $ do
      withApp (Scotty.post "/search" $ formParam "query" >>= text) $ do
        it "returns form parameter with given name" $ do
          postForm "/search" "query=haskell" `shouldRespondWith` "haskell"

        it "replaces non UTF-8 bytes with Unicode replacement character" $ do
          postForm "/search" "query=\xe9" `shouldRespondWith` "\xfffd"

        it "decodes URL-encoding" $ do
          postForm "/search" "query=Kurf%C3%BCrstendamm" `shouldRespondWith` "Kurfürstendamm"
      withApp (Scotty.post "/search" (do
                                             v <- formParam "query"
                                             json (v :: Int))) $ do
        it "responds with 200 OK if the form parameter can be parsed at the right type" $ do
          postForm "/search" "query=42" `shouldRespondWith` 200
        it "responds with 400 Bad Request if the form parameter cannot be parsed at the right type" $ do
          postForm "/search" "query=potato" `shouldRespondWith` 400
      withApp (Scotty.post "/checkbox" (do
                                             e <- formParam "enabled"
                                             json (e :: Bool))) $ do
        it "responds with 200 OK if the checkbox parameter can be parsed at the right type" $ do
          postForm "/checkbox" "enabled=true" `shouldRespondWith` 200
          postForm "/checkbox" "enabled=on" `shouldRespondWith` 200
          postForm "/checkbox" "enabled=false" `shouldRespondWith` 200
        it "responds with 400 Bad Request if the checkbox parameter cannot be parsed at the right type" $ do
          postForm "/checkbox" "enabled=undefined" `shouldRespondWith` 400

      withApp (do
                  Scotty.post "/" $ next
                  Scotty.post "/" $ do
                    p :: Int <- formParam "p"
                    json p
              ) $ do
        it "preserves the body of a POST request even after 'next' (#147)" $ do
          postForm "/" "p=42" `shouldRespondWith` "42"
      context "recover from type mismatch parameter exception" $ do
        withApp (Scotty.post "/search" $
                 (formParam "z" >>= (\v -> json (v :: Int))) `catch` (\(_::ScottyException) -> text "z")
                ) $ do
          it "catches a StatusError" $ do
            postForm "/search" "z=potato" `shouldRespondWith` 200 { matchBody = "z"}

    describe "captureParamMaybe" $ do
      withApp (
        do
          Scotty.get "/a/:q" $ do
            mx <- captureParamMaybe "q"
            case mx of
              Just (_ :: Int) -> status status200
              Nothing -> status status500
          Scotty.get "/b/:q" $ do
            mx <- captureParamMaybe "z"
            case mx of
              Just (_ :: TL.Text) -> text "impossible" >> status status500
              Nothing -> status status200
              ) $ do
        it "responds with 200 OK if the parameter can be parsed at the right type, 500 otherwise" $ do
          get "/a/potato" `shouldRespondWith` 500
          get "/a/42" `shouldRespondWith` 200
        it "responds with 200 OK if the parameter is not found" $ do
          get "/b/potato" `shouldRespondWith` 200

    describe "files" $ do
      withApp (Scotty.post "/files" $ do
                  fs <- files
                  text $ TL.pack $ show $ length fs) $ do
        context "small number of files" $ do
          it "loads uploaded files in memory" $ do
            postMultipartForm "/files" "ABC123" [
              (FMFile "file1.txt", "text/plain;charset=UTF-8", "first_file", "xxx")
              ] `shouldRespondWith` 200 { matchBody = "1"}
        context "file name too long (> 32 bytes)" $ do
          it "responds with 413 - Request Too Large" $ do
            postMultipartForm "/files" "ABC123" [
              (FMFile "file.txt", "text/plain;charset=UTF-8", "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzx", "xxx")
                                                ] `shouldRespondWith` 413
        context "large number of files (> 10)" $ do
          it "responds with 413 - Request Too Large" $ do
            postMultipartForm "/files" "ABC123" [
              (FMFile "file1.txt", "text/plain;charset=UTF-8", "file", "xxx"),
              (FMFile "file1.txt", "text/plain;charset=UTF-8", "file", "xxx"),
              (FMFile "file1.txt", "text/plain;charset=UTF-8", "file", "xxx"),
              (FMFile "file1.txt", "text/plain;charset=UTF-8", "file", "xxx"),
              (FMFile "file1.txt", "text/plain;charset=UTF-8", "file", "xxx"),
              (FMFile "file1.txt", "text/plain;charset=UTF-8", "file", "xxx"),
              (FMFile "file1.txt", "text/plain;charset=UTF-8", "file", "xxx"),
              (FMFile "file1.txt", "text/plain;charset=UTF-8", "file", "xxx"),
              (FMFile "file1.txt", "text/plain;charset=UTF-8", "file", "xxx"),
              (FMFile "file1.txt", "text/plain;charset=UTF-8", "file", "xxx"),
              (FMFile "file1.txt", "text/plain;charset=UTF-8", "file", "xxx")
              ] `shouldRespondWith` 413


    describe "filesOpts" $ do
      let
          postMpForm = postMultipartForm "/files" "ABC123" [
            (FMFile "file1.txt", "text/plain;charset=UTF-8", "first_file", "xxx"),
            (FMFile "file2.txt", "text/plain;charset=UTF-8", "second_file", "yyy")
            ]
          processForm = do
            filesOpts defaultParseRequestBodyOptions $ \_ fs -> do
              text $ TL.pack $ show $ length fs
      withApp (Scotty.post "/files" processForm
              ) $ do
        it "loads uploaded files in memory" $ do
          postMpForm `shouldRespondWith` 200 { matchBody = "2"}
      context "preserves the body of a POST request even after 'next' (#147)" $ do
        withApp (do
                    Scotty.post "/files" next
                    Scotty.post "/files" processForm) $ do
          it "loads uploaded files in memory" $ do
            postMpForm `shouldRespondWith` 200 { matchBody = "2"}


    describe "text" $ do
      let modernGreekText :: IsString a => a
          modernGreekText = "νέα ελληνικά"

      withApp (Scotty.get "/scotty" $ text modernGreekText) $ do
        it "sets body to given text" $ do
          get "/scotty" `shouldRespondWith` modernGreekText

        it "sets Content-Type header to \"text/plain; charset=utf-8\"" $ do
          get "/scotty" `shouldRespondWith` 200 {matchHeaders = ["Content-Type" <:> "text/plain; charset=utf-8"]}

      withApp (Scotty.get "/scotty" $ setHeader "Content-Type" "text/somethingweird" >> text modernGreekText) $ do
        it "doesn't override a previously set Content-Type header" $ do
          get "/scotty" `shouldRespondWith` 200 {matchHeaders = ["Content-Type" <:> "text/somethingweird"]}

    describe "html" $ do
      let russianLanguageTextInHtml :: IsString a => a
          russianLanguageTextInHtml = "<p>ру́сский язы́к</p>"

      withApp (Scotty.get "/scotty" $ html russianLanguageTextInHtml) $ do
        it "sets body to given text" $ do
          get "/scotty" `shouldRespondWith` russianLanguageTextInHtml

        it "sets Content-Type header to \"text/html; charset=utf-8\"" $ do
          get "/scotty" `shouldRespondWith` 200 {matchHeaders = ["Content-Type" <:> "text/html; charset=utf-8"]}

      withApp (Scotty.get "/scotty" $ setHeader "Content-Type" "text/somethingweird" >> html russianLanguageTextInHtml) $ do
        it "doesn't override a previously set Content-Type header" $ do
          get "/scotty" `shouldRespondWith` 200 {matchHeaders = ["Content-Type" <:> "text/somethingweird"]}

    describe "json" $ do
      withApp (Scotty.get "/scotty" $ setHeader "Content-Type" "text/somethingweird" >> json (Just (5::Int))) $ do
        it "doesn't override a previously set Content-Type header" $ do
          get "/scotty" `shouldRespondWith` 200 {matchHeaders = ["Content-Type" <:> "text/somethingweird"]}

    describe "finish" $ do
      withApp (Scotty.get "/scotty" $ finish) $ do
        it "responds with 200 by default" $ do
          get "/scotty" `shouldRespondWith` 200

      withApp (Scotty.get "/scotty" $ status status400 >> finish >> status status200) $ do
        it "stops the execution of an action" $ do
          get "/scotty" `shouldRespondWith` 400

    describe "setSimpleCookie" $ do
      withApp (Scotty.get "/scotty" $ SC.setSimpleCookie "foo" "bar") $ do
        it "responds with a Set-Cookie header" $ do
          get "/scotty" `shouldRespondWith` 200 {matchHeaders = ["Set-Cookie" <:> "foo=bar"]}

    describe "getCookie" $ do
      withApp (Scotty.get "/scotty" $ do
                 mt <- SC.getCookie "foo"
                 case mt of
                   Just "bar" -> Scotty.status status200
                   _ -> Scotty.status status400 ) $ do
        it "finds the right cookie in the request headers" $ do
          request "GET" "/scotty" [("Cookie", "foo=bar")] "" `shouldRespondWith` 200

    describe "deleteCookie" $ do
      withApp (Scotty.get "/scotty" $ SC.deleteCookie "foo") $ do
        it "responds with a Set-Cookie header with expiry date Jan 1, 1970" $ do
          get "/scotty" `shouldRespondWith` 200 {matchHeaders = ["Set-Cookie" <:> "foo=; Expires=Thu, 01-Jan-1970 00:00:00 GMT"]}

    describe "nested" $ do
      let
        simpleApp :: Application
        simpleApp _ respond = do
            putStrLn "I've done some IO here"
            respond $ responseLBS
                status200
                [("Content-Type", "text/plain")]
                "Hello, Web!"

      withApp (Scotty.get "/nested" (nested simpleApp)) $ do
        it "responds with the expected simpleApp response" $ do
          get "/nested" `shouldRespondWith` 200 {matchHeaders = ["Content-Type" <:> "text/plain"], matchBody = "Hello, Web!"}
    
    describe "Session Management" $ do
      withApp (Scotty.get "/scotty" $ do
                 sessionJar <- liftIO createSessionJar 
                 sess <- createUserSession sessionJar Nothing ("foo" :: T.Text)
                 mRes <- readSession sessionJar (sessId sess)
                 case mRes of
                   Left _ -> Scotty.status status400
                   Right res -> do 
                     if res /= "foo" then Scotty.status status400
                     else text "all good"
                        ) $ do
        it "Roundtrip of session by adding and fetching a value" $ do
          get "/scotty" `shouldRespondWith` 200

-- Unix sockets not available on Windows
#if !defined(mingw32_HOST_OS)
  describe "scottySocket" .
    it "works with a unix socket" .
      withServer (Scotty.get "/scotty" $ html "") .
        E.bracket (socket AF_UNIX Stream 0) close $ \sock -> do
          connect sock $ SockAddrUnix socketPath
          _ <- send sock "GET /scotty HTTP/1.1\r\n\n"
          r1 <- recv sock 1024
          _ <- send sock "GET /four-oh-four HTTP/1.1\r\n\n"
          r2 <- recv sock 1024
          (BS.take (BS.length ok) r1, BS.take (BS.length no) r2) `shouldBe` (ok, no)
  where ok, no :: ByteString
        ok = "HTTP/1.1 200 OK"
        no = "HTTP/1.1 404 Not Found"

socketPath :: FilePath
socketPath = "/tmp/scotty-test.socket"

withServer :: ScottyM () -> IO a -> IO a
withServer actions inner = E.bracket
  (listenOn socketPath)
  (\sock -> close sock >> removeFile socketPath)
  (\sock -> withAsync (Scotty.scottySocket defaultOptions sock actions) $ const inner)

-- See https://github.com/haskell/network/issues/318
listenOn :: String -> IO Socket
listenOn path =
  bracketOnError
    (socket AF_UNIX Stream 0)
    close
    (\sock -> do
      setSocketOption sock ReuseAddr 1
      bind sock (SockAddrUnix path)
      listen sock maxListenQueue
      return sock
    )
#endif

