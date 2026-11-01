{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson (object, (.=))
import qualified Data.Text as T
import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
    -- Customize API metadata displayed in Swagger UI
    setDocsTitle "Scotty Swagger Demo API"
    setDocsVersion "1.0.0"
    setDocsDescription "FastAPI-style interactive Swagger UI documentation baked into Scotty!"

    -- Standard routes are automatically documented at /docs and /openapi.json
    get "/" $ do
        text "Welcome! Visit http://localhost:3000/docs for Swagger UI or http://localhost:3000/openapi.json for the OpenAPI schema."

    -- Documented route with tags and summary
    getDoc (doc $ summary "List all items" . tags ["Items"]) "/items" $ do
        json [ object [ "id" .= ("item-1" :: T.Text), "name" .= ("First Item" :: T.Text) ]
             , object [ "id" .= ("item-2" :: T.Text), "name" .= ("Second Item" :: T.Text) ]
             ]

    -- Documented route with path parameter and tags
    getDoc (doc $ summary "Get item by ID" . tags ["Items"]) "/items/:id" $ do
        itemId <- pathParam "id"
        json $ object [ "id" .= (itemId :: T.Text), "name" .= ("Item " <> itemId) ]

    -- Documented POST route
    postDoc (doc $ summary "Create an item" . tags ["Items"]) "/items" $ do
        json $ object [ "status" .= ("created" :: T.Text) ]

    -- Documented route with query parameter
    getDoc (doc $ summary "Search items" . tags ["Items"] . paramDoc (queryParamDoc "query")) "/search" $ do
        q <- queryParamMaybe "query"
        json $ object [ "results" .= ([] :: [T.Text]), "query" .= (q :: Maybe T.Text) ]
