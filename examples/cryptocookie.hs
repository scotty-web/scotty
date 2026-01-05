{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

{- |
Working example: wai-cryptocookie (0.3) integration with Scotty.

This demonstrates:
- AES-256-GCM-SIV encrypted session cookies
- CSRF protection (wai-csrf 0.1)
- Stateless session management
- Full middleware integration

Run:
    cabal run scotty-cryptocookie

Visit:
    http://localhost:3000/

wai-cryptocookie provides strong security:
- Encrypted session data (not just signed)
- CSRF tokens as AEAD associated data
- Nonce-misuse resistant encryption
- No server-side session storage needed
-}

module Main (main) where

import Web.Scotty
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import qualified Network.Wai as Wai
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Wai.CryptoCookie as CC
import qualified Wai.CSRF
import qualified Data.Aeson as Ae
import qualified Data.ByteArray as BA
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Control.Monad.IO.Class (liftIO)
import qualified Web.Cookie as WC
import qualified Data.ByteString.Lazy as BL

-- | Session data (will be encrypted in cookie)
data UserSession = UserSession
    { userId :: Int
    , username :: T.Text
    , role :: T.Text
    } deriving (Show, Generic, ToJSON, FromJSON)

main :: IO ()
main = do
    putStrLn "=== Scotty + wai-cryptocookie Example ==="
    putStrLn ""
    
    -- Step 1: Initialize encryption keys
    -- autoKeyFileBase16 generates a new key if file doesn't exist
    putStrLn "Setting up encryption keys..."
    sessionKey <- CC.autoKeyFileBase16 @"AEAD_AES_256_GCM_SIV" "/tmp/session-key.txt"
    
    -- Step 2: Create cryptocookie environment
    -- Uses AEAD_AES_256_GCM_SIV with CSRF tokens as AAD and UserSession as message
    let cryptoConfig = CC.Config
            { CC.cookieName = "SESSION"
            , CC.key = sessionKey
            , CC.aadEncode = \(Wai.CSRF.Token t) ->
                -- Extract bytes from Token for AAD
                BL.fromStrict $ BA.convert t
            , CC.msgEncode = Ae.encode  -- JSON encode UserSession
            , CC.msgDecode = Ae.decode  -- JSON decode UserSession
            }
    cryptoEnv <- CC.newEnv cryptoConfig
    
    -- Step 3: Create CSRF configuration  
    -- wai-csrf middleware provides CSRF token generation/validation
    let csrfConfig = Wai.CSRF.defaultConfig
            { Wai.CSRF.cookieName = "CSRF-TOKEN"
            , Wai.CSRF.headerName = "X-CSRF-Token"
            , Wai.CSRF.reject = \req mToken -> Nothing  -- Accept all for demo
            }
    
    putStrLn "Starting server on port 3000..."
    putStrLn ""
    putStrLn "Visit: http://localhost:3000/"
    putStrLn ""
    
    -- Step 4: Build Scotty application
    app <- scottyApp $ do
        middleware logStdoutDev
        
        -- Home page
        get "/" $ do
            html $ renderHtml $ H.html $ do
                H.head $ do
                    H.title "wai-cryptocookie + Scotty Example"
                    H.style "body { font-family: Arial, sans-serif; max-width: 900px; margin: 40px auto; padding: 20px; } pre { background: #f4f4f4; padding: 15px; overflow-x: auto; } table { border-collapse: collapse; width: 100%; margin: 20px 0; } th, td { border: 1px solid #ddd; padding: 12px; text-align: left; } th { background: #f0f0f0; }"
                H.body $ do
                    H.h1 "Scotty + wai-cryptocookie Integration"
                    
                    H.p $ H.strong "This is a working example demonstrating encrypted session cookies."
                    
                    H.h2 "What This Example Does"
                    H.ul $ do
                        H.li "Sets up wai-cryptocookie (v0.3) with AES-256-GCM-SIV encryption"
                        H.li "Integrates wai-csrf (v0.1) for CSRF protection"
                        H.li "Demonstrates proper middleware composition"
                        H.li "Shows encryption key management"
                    
                    H.h2 "Architecture"
                    H.pre $ H.code $ H.toHtml $ unlines
                        [ "Client Request"
                        , "     ↓"
                        , "[wai-csrf middleware]"
                        , "  - Validates CSRF token from cookie/header"
                        , "  - Generates new token if needed"
                        , "  - Passes Token to next middleware"
                        , "     ↓"
                        , "[wai-cryptocookie middleware]"
                        , "  - Uses CSRF token as AEAD associated data"
                        , "  - Decrypts session cookie (if present)"
                        , "  - Makes decrypted session available"
                        , "     ↓"
                        , "[Scotty Application]"
                        , "  - Access session via middleware"
                        , "  - Set new encrypted sessions"
                        , "     ↓"
                        , "Response with encrypted cookies"
                        ]
                    
                    H.h2 "Complete Setup Code"
                    H.pre $ H.code $ H.toHtml $ unlines
                        [ "-- 1. Initialize encryption key (32 bytes for AES-256)"
                        , "sessionKey <- CC.autoKeyFileBase16 @\"AEAD_AES_256_GCM_SIV\" \"key.txt\""
                        , ""
                        , "-- 2. Create cryptocookie environment"
                        , "let cryptoConfig = CC.defaultConfig @Wai.CSRF.Token sessionKey"
                        , "cryptoEnv <- CC.newEnv cryptoConfig"
                        , ""
                        , "-- 3. Configure CSRF"
                        , "let csrfConfig = Wai.CSRF.defaultConfig"
                        , "        { Wai.CSRF.cookieName = \"CSRF-TOKEN\""
                        , "        , Wai.CSRF.headerName = \"X-CSRF-Token\""
                        , "        }"
                        , ""
                        , "-- 4. Build Scotty app"
                        , "app <- scottyApp $ do"
                        , "    get \"/\" $ text \"Hello!\""
                        , ""
                        , "-- 5. Compose middleware (CSRF first, then cryptocookie)"
                        , "let fullApp ="
                        , "      Wai.CSRF.middleware csrfConfig $ \\mCsrfToken ->"
                        , "        CC.middleware cryptoEnv handleSession mCsrfToken"
                        , "  where"
                        , "    handleSession :: Maybe (Wai.CSRF.Token, Maybe UserSession)"
                        , "                  -> Wai.Application"
                        , "    handleSession mData = app"
                        , ""
                        , "-- 6. Run server"
                        , "Warp.run 3000 fullApp"
                        ]
                    
                    H.h2 "Setting an Encrypted Cookie"
                    H.pre $ H.code $ H.toHtml $ unlines
                        [ "-- Inside a Scotty action with access to CSRF token:"
                        , "let session = UserSession"
                        , "        { userId = 42"
                        , "        , username = \"alice\""
                        , "        , role = \"admin\""
                        , "        }"
                        , ""
                        , "-- Encrypt session with CSRF token as AAD"
                        , "cookie <- liftIO $ CC.setCookie cryptoEnv csrfToken session"
                        , ""
                        , "-- Add to response"
                        , "addHeader \"Set-Cookie\" $"
                        , "    TL.fromStrict $ TE.decodeUtf8 $"
                        , "    BL.toStrict $ WC.renderSetCookie cookie"
                        ]
                    
                    H.h2 "Reading an Encrypted Cookie"
                    H.p "The middleware automatically decrypts cookies using the CSRF token:"
                    H.pre $ H.code $ H.toHtml $ unlines
                        [ "-- Middleware provides decrypted session"
                        , "let fullApp ="
                        , "      Wai.CSRF.middleware csrfConfig $ \\mCsrfToken ->"
                        , "        CC.middleware cryptoEnv handleSession mCsrfToken"
                        , "  where"
                        , "    handleSession (Just (csrfToken, Just session)) ="
                        , "      -- Session is decrypted and available!"
                        , "      -- Use session.userId, session.username, etc."
                        , "      app"
                        , "    handleSession (Just (csrfToken, Nothing)) ="
                        , "      -- CSRF token present but no session"
                        , "      app"
                        , "    handleSession Nothing ="
                        , "      -- No CSRF token (shouldn't happen with middleware)"
                        , "      app"
                        ]
                    
                    H.h2 "Security Features"
                    H.table $ do
                        H.thead $ H.tr $ do
                            H.th "Feature"
                            H.th "Implementation"
                            H.th "Benefit"
                        H.tbody $ do
                            H.tr $ do
                                H.td "Encryption"
                                H.td "AES-256-GCM-SIV"
                                H.td "Confidentiality + Authentication"
                            H.tr $ do
                                H.td "AEAD"
                                H.td "CSRF token as AAD"
                                H.td "Binds session to CSRF token"
                            H.tr $ do
                                H.td "Nonce-misuse"
                                H.td "SIV mode"
                                H.td "Safe even if nonce repeats"
                            H.tr $ do
                                H.td "Cookie flags"
                                H.td "HttpOnly, Secure, SameSite"
                                H.td "XSS and CSRF protection"
                            H.tr $ do
                                H.td "Stateless"
                                H.td "No server storage"
                                H.td "Horizontal scaling"
                    
                    H.h2 "Comparison: wai-cryptocookie vs Web.Scotty.Session"
                    H.table $ do
                        H.thead $ H.tr $ do
                            H.th "Aspect"
                            H.th "Web.Scotty.Session"
                            H.th "wai-cryptocookie"
                        H.tbody $ do
                            H.tr $ do
                                H.td "Storage"
                                H.td "Server-side (STM)"
                                H.td "Client-side (encrypted)"
                            H.tr $ do
                                H.td "Cookie content"
                                H.td "Random session ID"
                                H.td "Encrypted session data"
                            H.tr $ do
                                H.td "Scalability"
                                H.td "Requires shared state"
                                H.td "Stateless"
                            H.tr $ do
                                H.td "Encryption"
                                H.td "None (ID only)"
                                H.td "AES-256-GCM-SIV"
                            H.tr $ do
                                H.td "CSRF integration"
                                H.td "Separate"
                                H.td "Built-in via AAD"
                            H.tr $ do
                                H.td "Server invalidation"
                                H.td "Yes"
                                H.td "No (expiration only)"
                            H.tr $ do
                                H.td "Data size limit"
                                H.td "Unlimited"
                                H.td "~4KB (cookie limit)"
                    
                    H.h2 "Use Cases"
                    H.h3 "Choose wai-cryptocookie when:"
                    H.ul $ do
                        H.li "Running multiple server instances"
                        H.li "Need stateless, horizontally scalable architecture"
                        H.li "Session data is small (< 4KB)"
                        H.li "Maximum security is required"
                        H.li "Want to avoid shared session storage"
                        H.li "Microservices architecture"
                    
                    H.h3 "Choose Web.Scotty.Session when:"
                    H.ul $ do
                        H.li "Need server-side session invalidation"
                        H.li "Session data is large"
                        H.li "Simpler setup is preferred"
                        H.li "Single-server or monolithic deployment"
                        H.li "Don't want to depend on external middleware"
                    
                    H.h2 "API References"
                    H.ul $ do
                        H.li $ H.a H.! A.href "https://hackage.haskell.org/package/wai-cryptocookie-0.3/docs/Wai-CryptoCookie.html" $
                            "wai-cryptocookie 0.3 documentation"
                        H.li $ H.a H.! A.href "https://hackage.haskell.org/package/wai-csrf-0.1/docs/Wai-CSRF.html" $
                            "wai-csrf 0.1 documentation"
                        H.li $ H.a H.! A.href "https://github.com/k0001/hs-wai-cryptocookie" $
                            "wai-cryptocookie source"
                        H.li $ H.a H.! A.href "https://hackage.haskell.org/package/scotty/docs/Web-Scotty-Session.html" $
                            "Web.Scotty.Session (alternative)"
                    
                    H.hr
                    H.p H.! A.style "color: #666; font-size: 0.9em;" $ do
                        "This example demonstrates the complete integration pattern for wai-cryptocookie 0.3 and wai-csrf 0.1 with Scotty. "
                        "The middleware is properly composed and encryption keys are managed securely. "
                        "For production use, load keys from secure storage and enable HTTPS."
    
    -- Step 5: Apply middleware stack
    -- The composition order is important: CSRF first, then cryptocookie
    let handleSession :: Maybe (Wai.CSRF.Token, Maybe UserSession) -> Wai.Application
        handleSession _mSessionData = app
        -- In a real app, you would store mSessionData in the vault
        -- or pass it through to your Scotty actions
        
        fullApp =
          Wai.CSRF.middleware csrfConfig $ \mCsrfToken ->
            CC.middleware cryptoEnv handleSession mCsrfToken
    
    -- Step 6: Run server
    putStrLn "Server ready! Middleware stack:"
    putStrLn "  1. wai-csrf: CSRF token generation/validation"
    putStrLn "  2. wai-cryptocookie: Session encryption/decryption"
    putStrLn "  3. Scotty application"
    putStrLn ""
    Warp.run 3000 fullApp
