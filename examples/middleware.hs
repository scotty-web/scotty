{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.ValidateHeaders (validateHeadersMiddleware, defaultValidateHeadersSettings)
import Data.Aeson (object, (.=))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
    putStrLn "\n=== Scotty Middleware Example ==="
    putStrLn "\nThis example demonstrates two WAI middlewares:"
    putStrLn "1. Request Logging - logs all HTTP requests to stdout"
    putStrLn "2. Header Validation - validates incoming request headers\n"
    putStrLn "The validateHeadersMiddleware checks REQUEST headers from clients."
    putStrLn "It rejects requests with invalid headers (containing illegal characters)"
    putStrLn "such as CR/LF, control characters, or invalid header names.\n"
    putStrLn "Try these requests:"
    putStrLn "  curl http://localhost:3000/"
    putStrLn "  curl -H 'X-Custom: value' http://localhost:3000/headers"
    putStrLn "  curl -H 'X-Token!#$: valid' http://localhost:3000/headers"
    putStrLn "\nTo test invalid headers (these will return 500):"
    putStrLn "  curl -H 'Invalid Header: value' http://localhost:3000/  # header name cannot contain spaces"
    putStrLn "  Note: Headers with CR/LF in values will also be rejected"
    putStrLn "\nPress Ctrl+C to stop\n"
    
    scotty 3000 $ do
        -- Request logging middleware - logs all requests to stdout in development format
        middleware logStdoutDev
        
        -- Header validation middleware - validates REQUEST headers to prevent header injection
        -- This middleware checks headers sent by the CLIENT and rejects malformed ones
        -- It validates both header names and values according to HTTP specifications
        middleware $ validateHeadersMiddleware defaultValidateHeadersSettings
        
        -- Endpoint group 1: Basic endpoints that demonstrate logging
        get "/" $ do
            text "Welcome! This request was logged by the logging middleware."
        
        get "/hello/:name" $ do
            name <- pathParam "name"
            text $ "Hello, " <> name <> "! Check the server logs to see the request details."
        
        -- Endpoint group 2: Shows all request headers that passed validation
        get "/headers" $ do
            allHeaders <- headers
            let formatHeader (name, value) = 
                    TL.fromStrict name <> ": " <> TL.fromStrict value <> "\n"
                headerList = map formatHeader allHeaders
            text $ mconcat 
                [ "Your request headers (all validated by middleware):\n\n"
                , mconcat headerList
                , "\nThese headers passed validation. Invalid headers would have been rejected with 500.\n"
                , "Try sending a request with invalid headers to see the middleware in action."
                ]
        
        -- Endpoint group 3: Demonstrates specific header inspection
        get "/user-agent" $ do
            agent <- header "User-Agent"
            case agent of
                Just ua -> text $ "Your User-Agent: " <> ua
                Nothing -> text "No User-Agent header provided"
        
        -- Endpoint group 4: POST request to demonstrate logging of different methods
        post "/echo" $ do
            requestBody <- body
            text $ "Request body received (" <> TL.pack (show (BL.length requestBody)) <> " bytes). Check server logs for details."
        
        -- Endpoint group 5: JSON response with logging
        get "/json" $ do
            json $ object 
                [ "message" .= ("All request headers were validated" :: String)
                , "middleware" .= object
                    [ "logging" .= ("logStdoutDev" :: String)
                    , "validation" .= ("validateHeadersMiddleware" :: String)
                    ]
                ]
