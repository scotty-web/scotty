{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.ValidateHeaders
import Network.HTTP.Types (status400)
import Data.Aeson (object, (.=))
import qualified Data.Text.Lazy as TL

main :: IO ()
main = scotty 3000 $ do
    -- Request logging middleware - logs all requests to stdout in development format
    middleware logStdoutDev
    
    -- Header validation middleware - validates that certain headers are present
    -- This will reject requests that don't have valid headers with a 400 response
    middleware $ validateHeaders ["User-Agent"]
    
    -- Endpoint group 1: Basic endpoints that demonstrate logging
    get "/" $ do
        text "Welcome! This request was logged by the logging middleware."
    
    get "/hello/:name" $ do
        name <- pathParam "name"
        text $ "Hello, " <> name <> "! Check the server logs to see the request details."
    
    -- Endpoint group 2: Demonstrates header validation
    get "/protected" $ do
        text "This endpoint requires a User-Agent header (validated by middleware)."
    
    -- Endpoint group 3: Multiple headers demonstration
    get "/headers" $ do
        agent <- header "User-Agent"
        case agent of
            Just ua -> html $ mconcat 
                [ "<h1>Header Information</h1>"
                , "<p>Your User-Agent: ", ua, "</p>"
                , "<p>This request was validated and logged by the middleware chain.</p>"
                ]
            Nothing -> do
                status status400
                text "User-Agent header is required"
    
    -- Endpoint group 4: POST request to demonstrate logging of different methods
    post "/echo" $ do
        b <- body
        text "Request body received and logged. Check server logs for details."
    
    -- Endpoint group 5: JSON response with logging
    get "/json" $ do
        json $ object ["message" .= ("This JSON response was logged" :: TL.Text)]
