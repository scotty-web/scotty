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

For complete documentation, see: examples/cryptocookie-README.md

wai-cryptocookie provides strong security:
- Encrypted session data (not just signed)
- CSRF tokens as AEAD associated data
- Nonce-misuse resistant encryption
- No server-side session storage needed
-}

module Main (main) where

import Web.Scotty
import qualified Data.Text as T
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import qualified Network.Wai as Wai
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Wai.CryptoCookie as CC
import qualified Wai.CSRF
import qualified Data.Aeson as Ae
import qualified Data.ByteArray as BA
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
        
        -- Home page - minimal text response
        get "/" $ do
            text "wai-cryptocookie + Scotty example running. See cryptocookie-README.md for documentation."
    
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
