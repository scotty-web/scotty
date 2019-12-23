{-# LANGUAGE OverloadedStrings, CPP #-}
module Web.ScottySpec (main, spec) where

import           Test.Hspec
import           Test.Hspec.Wai

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.String
import           Network.HTTP.Types
import qualified Control.Exception.Lifted as EL
import qualified Control.Exception as E

import           Web.Scotty as Scotty hiding (get, post, put, patch, delete, request, options)
import qualified Web.Scotty as Scotty

#if !defined(mingw32_HOST_OS)
import           Control.Concurrent.Async (withAsync)
import           Control.Exception (bracketOnError)
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import           Data.Default.Class (def)
import           Network.Socket (Family(..), SockAddr(..), Socket, SocketOption(..), SocketType(..), bind, close, connect, listen, maxListenQueue, setSocketOption, socket)
import           Network.Socket.ByteString (send, recv)
import           System.Directory (removeFile)
#endif

main :: IO ()
main = hspec spec

availableMethods :: [StdMethod]
availableMethods = [GET, POST, HEAD, PUT, PATCH, DELETE, OPTIONS]

spec :: Spec
spec = do
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
      withApp (defaultHandler text >> Scotty.get "/" (liftAndCatchIO $ E.throwIO E.DivideByZero)) $ do
        it "sets custom exception handler" $ do
          get "/" `shouldRespondWith` "divide by zero" {matchStatus = 500}

      withApp (defaultHandler (\_ -> status status503) >> Scotty.get "/" (liftAndCatchIO $ E.throwIO E.DivideByZero)) $ do
        it "allows to customize the HTTP status code" $ do
          get "/" `shouldRespondWith` "" {matchStatus = 503}

      context "when not specified" $ do
        withApp (Scotty.get "/" $ liftAndCatchIO $ E.throwIO E.DivideByZero) $ do
          it "returns 500 on exceptions" $ do
            get "/" `shouldRespondWith` "<h1>500 Internal Server Error</h1>divide by zero" {matchStatus = 500}

  describe "ActionM" $ do
    withApp (Scotty.get "/" $ (undefined `EL.catch` ((\_ -> html "") :: E.SomeException -> ActionM ()))) $ do
      it "has a MonadBaseControl instance" $ do
        get "/" `shouldRespondWith` 200

    withApp (Scotty.get "/dictionary" $ empty <|> param "word1" <|> empty <|> param "word2" >>= text) $
      it "has an Alternative instance" $ do
        get "/dictionary?word1=haskell"   `shouldRespondWith` "haskell"
        get "/dictionary?word2=scotty"    `shouldRespondWith` "scotty"
        get "/dictionary?word1=a&word2=b" `shouldRespondWith` "a"

    describe "param" $ do
      withApp (Scotty.matchAny "/search" $ param "query" >>= text) $ do
        it "returns query parameter with given name" $ do
          get "/search?query=haskell" `shouldRespondWith` "haskell"

        context "when used with application/x-www-form-urlencoded data" $ do
          it "returns POST parameter with given name" $ do
            request "POST" "/search" [("Content-Type","application/x-www-form-urlencoded")] "query=haskell" `shouldRespondWith` "haskell"

          it "replaces non UTF-8 bytes with Unicode replacement character" $ do
            request "POST" "/search" [("Content-Type","application/x-www-form-urlencoded")] "query=\xe9" `shouldRespondWith` "\xfffd"

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

        withApp = with . scottyApp

socketPath :: FilePath
socketPath = "/tmp/scotty-test.socket"

withServer :: ScottyM () -> IO a -> IO a
withServer actions inner = E.bracket
  (listenOn socketPath)
  (\sock -> close sock >> removeFile socketPath)
  (\sock -> withAsync (Scotty.scottySocket def sock actions) $ const inner)

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
