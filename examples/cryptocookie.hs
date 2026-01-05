{-# LANGUAGE OverloadedStrings #-}

{- |
Example demonstrating wai-cryptocookie for secure session management in Scotty.

This example shows how to integrate wai-cryptocookie which provides:
- Encrypted cookie values (not just signed)  
- Built-in cryptographic security using the crypton library
- Better protection against tampering and inspection
- Automatic cookie expiration and renewal

wai-cryptocookie improves upon basic session implementations by:
1. Encrypting the entire cookie content (vs. just signing it)
2. Using authenticated encryption (AES-GCM) preventing both reading and tampering
3. Built-in key derivation and rotation support
4. Automatic handling of cookie attributes (HttpOnly, Secure, SameSite)

Run this example with:
    cabal run scotty-cryptocookie

Then visit:
    http://localhost:3000/

Note: This is a demonstration of the integration pattern. In production:
- Load encryption keys from environment variables or secure key management
- Enable HTTPS and set Secure cookie flag
- Implement proper key rotation
- Use appropriate session timeout values
-}

module Main (main) where

import Web.Scotty
import qualified Data.Text.Lazy as TL
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

main :: IO ()
main = do
    putStrLn "=== Scotty wai-cryptocookie Integration Example ==="
    putStrLn ""
    putStrLn "This example demonstrates how to integrate wai-cryptocookie"
    putStrLn "with Scotty for encrypted session management."
    putStrLn ""
    putStrLn "Starting server on port 3000..."
    putStrLn "Visit: http://localhost:3000/"
    putStrLn ""
    
    scotty 3000 $ do
        middleware logStdoutDev
        
        -- Home page explaining the integration
        get "/" $ do
            html $ renderHtml $ H.html $ do
                H.head $ do
                    H.title "wai-cryptocookie Integration Example"
                    H.style "body { font-family: Arial, sans-serif; max-width: 800px; margin: 50px auto; padding: 20px; }"
                H.body $ do
                    H.h1 "Scotty + wai-cryptocookie"
                    
                    H.h2 "What is wai-cryptocookie?"
                    H.p "wai-cryptocookie is a Haskell library that provides encrypted cookies for WAI applications."
                    H.p "It improves upon basic cookie/session implementations with:"
                    H.ul $ do
                        H.li $ H.strong "Encryption: " >> "Cookie values are encrypted, not just signed"
                        H.li $ H.strong "Authentication: " >> "Uses AES-GCM for authenticated encryption"
                        H.li $ H.strong "Security: " >> "Prevents both reading and tampering"
                        H.li $ H.strong "Standards: " >> "Proper HttpOnly, Secure, and SameSite attributes"
                        H.li $ H.strong "Key Management: " >> "Built-in key derivation and rotation"
                    
                    H.h2 "Integration Pattern"
                    H.p "To integrate wai-cryptocookie with Scotty:"
                    
                    H.h3 "1. Add Dependency"
                    H.pre $ H.code $ H.toHtml 
                        ("-- In your .cabal file:\n" :: String)
                        >> H.toHtml ("build-depends: wai-cryptocookie >= 0.3\n" :: String)
                    
                    H.h3 "2. Initialize Settings"
                    H.pre $ H.code $ H.toHtml $ unlines
                        [ "import qualified Wai.CryptoCookie as CC"
                        , ""
                        , "initCryptoCookie :: IO (CC.Settings, CC.State)"
                        , "initCryptoCookie = do"
                        , "    -- Load key from environment in production"
                        , "    let key = ... -- 32 bytes for AES-256"
                        , "    "
                        , "    let settings = CC.defaultSettings"
                        , "            { CC.setCookieName = \"session\""
                        , "            , CC.setSecretKey = CC.secretKeyFromByteString key"
                        , "            , CC.setMaxAge = Just 3600  -- 1 hour"
                        , "            , CC.setHttpOnly = True"
                        , "            , CC.setSecure = True  -- Requires HTTPS"
                        , "            , CC.setSameSite = CC.SameSiteLax"
                        , "            }"
                        , "    "
                        , "    state <- CC.mkState settings"
                        , "    return (settings, state)"
                        ]
                    
                    H.h3 "3. Apply Middleware"
                    H.pre $ H.code $ H.toHtml $ unlines
                        [ "main :: IO ()"
                        , "main = do"
                        , "    (ccSettings, ccState) <- initCryptoCookie"
                        , "    "
                        , "    -- Get WAI application from Scotty"
                        , "    app <- scottyApp $ do"
                        , "        get \"/\" $ do"
                        , "            text \"Hello!\""
                        , "    "
                        , "    -- Wrap with cryptocookie middleware"
                        , "    let appWithCrypto = CC.middleware ccSettings ccState app"
                        , "    "
                        , "    -- Run with Warp"
                        , "    Warp.run 3000 appWithCrypto"
                        ]
                    
                    H.h3 "4. Access Session Data"
                    H.pre $ H.code $ H.toHtml $ unlines
                        [ "-- In your Scotty actions:"
                        , "get \"/profile\" $ do"
                        , "    req <- request  -- Get WAI request"
                        , "    let vault = Wai.vault req"
                        , "    case CC.vaultLookup vault of"
                        , "        Nothing -> text \"Not logged in\""
                        , "        Just session -> text \"Welcome back!\""
                        ]
                    
                    H.h2 "Security Comparison"
                    H.p "Comparison with basic session implementation:"
                    H.table $ do
                        H.thead $ H.tr $ do
                            H.th "Feature"
                            H.th "Basic Sessions"
                            H.th "wai-cryptocookie"
                        H.tbody $ do
                            H.tr $ do
                                H.td "Storage"
                                H.td "Server-side in memory/STM"
                                H.td "Client-side encrypted"
                            H.tr $ do
                                H.td "Cookie Content"
                                H.td "Random session ID (visible)"
                                H.td "Encrypted data (opaque)"
                            H.tr $ do
                                H.td "Tampering Protection"
                                H.td "Via session ID lookup"
                                H.td "Authenticated encryption"
                            H.tr $ do
                                H.td "Confidentiality"
                                H.td "Session ID visible"
                                H.td "Full encryption"
                            H.tr $ do
                                H.td "Scalability"
                                H.td "Requires shared state"
                                H.td "Stateless (no server storage)"
                            H.tr $ do
                                H.td "Session Fixation"
                                H.td "Requires ID regeneration"
                                H.td "Built-in protection"
                    
                    H.h2 "When to Use Each"
                    H.h3 "Use wai-cryptocookie when:"
                    H.ul $ do
                        H.li "You need stateless session management"
                        H.li "You're running multiple server instances"
                        H.li "You want to avoid server-side session storage"
                        H.li "Session data is small and suitable for cookies"
                        H.li "You need maximum security for session data"
                    
                    H.h3 "Use basic sessions (Web.Scotty.Session) when:"
                    H.ul $ do
                        H.li "You need server-side session invalidation"
                        H.li "Session data is large"
                        H.li "You want simpler setup without middleware"
                        H.li "You're running a single-server application"
                    
                    H.h2 "Additional Resources"
                    H.ul $ do
                        H.li $ do
                            "Package: "
                            H.a H.! A.href "https://hackage.haskell.org/package/wai-cryptocookie" $ 
                                "wai-cryptocookie on Hackage"
                        H.li $ do
                            "Source: "
                            H.a H.! A.href "https://github.com/k0001/hs-wai-cryptocookie" $ 
                                "GitHub Repository"
                        H.li $ do
                            "Related: "
                            H.a H.! A.href "https://hackage.haskell.org/package/scotty/docs/Web-Scotty-Session.html" $ 
                                "Web.Scotty.Session Documentation"
                    
                    H.hr
                    H.p H.! A.style "color: #666; font-size: 0.9em;" $ do
                        "This is an informational example demonstrating the integration pattern. "
                        "For a working implementation, you need to properly initialize the middleware "
                        "and handle the WAI application lifecycle."

