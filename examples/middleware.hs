{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.ValidateHeaders (validateHeadersMiddleware, defaultValidateHeadersSettings)
import Data.Aeson (object, (.=))
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = scotty 3000 $ do
    -- Request logging middleware - logs all requests to stdout in development format
    middleware logStdoutDev
    
    -- Header validation middleware - validates header values to prevent header injection attacks
    -- This will reject responses with invalid header values (e.g., containing CR/LF)
    middleware $ validateHeadersMiddleware defaultValidateHeadersSettings
    
    -- Endpoint group 1: Basic endpoints that demonstrate logging
    get "/" $ do
        text "Welcome! This request was logged by the logging middleware."
    
    get "/hello/:name" $ do
        name <- pathParam "name"
        text $ "Hello, " <> name <> "! Check the server logs to see the request details."
    
    -- Endpoint group 2: Demonstrates safe header setting
    get "/safe-header" $ do
        setHeader "X-Custom-Header" "safe-value"
        text "This response has a safe custom header. Middleware validated it."
    
    -- Endpoint group 3: Multiple headers demonstration
    get "/headers" $ do
        agent <- header "User-Agent"
        case agent of
            Just ua -> html $ mconcat 
                [ "<h1>Header Information</h1>"
                , "<p>Your User-Agent: ", ua, "</p>"
                , "<p>This request was logged by the middleware chain.</p>"
                , "<p>Response headers are validated to prevent injection attacks.</p>"
                ]
            Nothing -> html "<h1>Header Information</h1><p>No User-Agent header found</p>"
    
    -- Endpoint group 4: POST request to demonstrate logging of different methods
    post "/echo" $ do
        requestBody <- body
        text $ "Request body received (" <> TL.pack (show (BL.length requestBody)) <> " bytes). Check server logs for details."
    
    -- Endpoint group 5: JSON response with logging
    get "/json" $ do
        json $ object ["message" .= ("This JSON response was logged" :: TL.Text)]
