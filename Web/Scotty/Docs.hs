{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

{- |
Module      : Web.Scotty.Docs
Copyright   : (c) 2026 Tushar Adhatrao, Scotty contributors
License     : BSD-3-Clause
Maintainer  :
Stability   : experimental
Portability : GHC

This module provides automatic OpenAPI 3.0 specification and Swagger UI
documentation for Scotty web applications, similar to FastAPI.

== Overview:

By default, every Scotty application automatically serves:
* @\/docs@: Interactive Swagger UI HTML documentation.
* @\/openapi.json@: OpenAPI 3.0 specification in JSON format.

== Example usage:

@
\{\-\# LANGUAGE OverloadedStrings \#\-\}

import Web.Scotty
import Web.Scotty.Docs

main :: IO ()
main = scotty 3000 $ do
    -- Standard routes are automatically documented in Swagger UI
    get "\/users" $ text "users list"

    -- Routes with path captures automatically generate OpenAPI path parameters
    getDoc (summary "Get user" . tags ["Users"]) "\/users\/:id" $ do
        uid <- pathParam "id"
        text $ "User: " <> uid
@

== Overriding and Customization:

* To override the @\/docs@ endpoint, define your own route @get \"\/docs\" ...@.
  User-defined routes take precedence over the built-in documentation endpoints.
* To disable documentation, use 'disableDocs' or set 'docsConfig' to 'Nothing' in 'Options'.
* To customize endpoints, title, or version, use 'setDocsConfig' or 'setDocsTitle'.
-}
module Web.Scotty.Docs
    ( -- * Configuration
      DocsConfig(..)
    , defaultDocsConfig
      -- * Route Documentation
    , RouteDoc(..)
    , defaultRouteDoc
    , ParamDoc(..)
    , ParamIn(..)
    , RouteInfo(..)
      -- * RouteDoc Combinators
    , doc
    , summary
    , description
    , tags
    , paramDoc
    , queryParamDoc
    , headerParamDoc
    , pathParamDoc
    , deprecated
    , operationId
      -- * Generation
    , swaggerUiHtml
    , openApiJson
    , patternToOpenApiPathAndParams
      -- * Application Integration
    , docApp
    ) where

import qualified Data.Aeson as A
import Data.Aeson ((.=))
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (StdMethod(..), status200)
import qualified Network.Wai as W

import Web.Scotty.Internal.Types
    ( DocsConfig(..)
    , defaultDocsConfig
    , ParamIn(..)
    , ParamDoc(..)
    , RouteDoc(..)
    , defaultRouteDoc
    , RouteInfo(..)
    , RoutePattern(..)
    , Application
    )

-- | Build a 'RouteDoc' using a composition of combinators starting from 'defaultRouteDoc'.
doc :: (RouteDoc -> RouteDoc) -> RouteDoc
doc f = f defaultRouteDoc

-- | Set the summary of a route.
summary :: T.Text -> RouteDoc -> RouteDoc
summary s d = d { rdSummary = Just s }

-- | Set the detailed description of a route.
description :: T.Text -> RouteDoc -> RouteDoc
description desc d = d { rdDescription = Just desc }

-- | Set tags for grouping routes in Swagger UI.
tags :: [T.Text] -> RouteDoc -> RouteDoc
tags ts d = d { rdTags = ts }

-- | Add a parameter documentation to a route.
paramDoc :: ParamDoc -> RouteDoc -> RouteDoc
paramDoc p d = d { rdParameters = rdParameters d ++ [p] }

-- | Create a query parameter documentation with default type "string".
queryParamDoc :: T.Text -> ParamDoc
queryParamDoc name = ParamDoc name ParamQuery False Nothing "string"

-- | Create a header parameter documentation with default type "string".
headerParamDoc :: T.Text -> ParamDoc
headerParamDoc name = ParamDoc name ParamHeader False Nothing "string"

-- | Create a path parameter documentation with required set to 'True'.
pathParamDoc :: T.Text -> ParamDoc
pathParamDoc name = ParamDoc name ParamPath True Nothing "string"

-- | Mark a route as deprecated in the OpenAPI spec.
deprecated :: RouteDoc -> RouteDoc
deprecated d = d { rdDeprecated = True }

-- | Set a unique operation ID for a route.
operationId :: T.Text -> RouteDoc -> RouteDoc
operationId opId d = d { rdOperationId = Just opId }

-- | Ensure path starts with a leading slash.
cleanPath :: T.Text -> T.Text
cleanPath p
    | T.null p = "/"
    | T.isPrefixOf "/" p = p
    | otherwise = "/" <> p

-- | Convert a 'RoutePattern' to an OpenAPI path format (e.g. @"/users/:id"@ to @"/users/{id}"@)
-- and extract any capture parameters.
patternToOpenApiPathAndParams :: RoutePattern -> (T.Text, [ParamDoc])
patternToOpenApiPathAndParams = \case
    Capture pat ->
        let segments = T.split (== '/') pat
            processSeg seg
                | Just (':', name) <- T.uncons seg =
                    ("{" <> name <> "}", [pathParamDoc name])
                | otherwise = (seg, [])
            (newSegs, paramsList) = unzip (map processSeg segments)
            rawPath = T.intercalate "/" newSegs
        in (cleanPath rawPath, concat paramsList)
    Literal pat ->
        (cleanPath pat, [])
    Function _ ->
        ("/custom-route", [])

-- | Convert a 'StdMethod' to its OpenAPI lowercase verb name.
methodToOpenApiText :: StdMethod -> T.Text
methodToOpenApiText = \case
    GET     -> "get"
    POST    -> "post"
    PUT     -> "put"
    DELETE  -> "delete"
    PATCH   -> "patch"
    OPTIONS -> "options"
    HEAD    -> "head"
    TRACE   -> "trace"
    CONNECT -> "connect"

-- | Convert a 'ParamDoc' to an OpenAPI Parameter Object JSON.
paramDocToJson :: ParamDoc -> A.Value
paramDocToJson ParamDoc{..} = A.object $ catMaybes
    [ Just ("name" .= pdName)
    , Just ("in" .= paramInToText pdIn)
    , Just ("required" .= pdRequired)
    , fmap ("description" .=) pdDescription
    , Just ("schema" .= A.object [ "type" .= pdSchemaType ])
    ]

paramInToText :: ParamIn -> T.Text
paramInToText = \case
    ParamPath   -> "path"
    ParamQuery  -> "query"
    ParamHeader -> "header"
    ParamCookie -> "cookie"

-- | Generate an OpenAPI 3.0.3 compliant JSON specification from the registered routes.
openApiJson :: DocsConfig -> [RouteInfo] -> A.Value
openApiJson cfg rDocs =
    let isDocRoute ri =
            let (p, _) = patternToOpenApiPathAndParams (riPattern ri)
            in p == cleanPath (docsPath cfg) || p == cleanPath (openApiPath cfg)
        userRoutes = filter (not . isDocRoute) rDocs

        routeToOps ri =
            let (pathStr, autoParams) = patternToOpenApiPathAndParams (riPattern ri)
                manualParams = rdParameters (riDoc ri)
                manualNames = [ pdName p | p <- manualParams ]
                unshadowedAuto = filter (\p -> pdName p `notElem` manualNames) autoParams
                allParams = unshadowedAuto ++ manualParams

                methods = case riMethod ri of
                    Just m  -> [methodToOpenApiText m]
                    Nothing -> ["get", "post", "put", "delete", "patch"]

                mkOp m =
                    let opSummary = fromMaybe (T.toUpper m <> " " <> pathStr) (rdSummary (riDoc ri))
                        fields = catMaybes
                            [ Just ("summary" .= opSummary)
                            , fmap ("description" .=) (rdDescription (riDoc ri))
                            , if null (rdTags (riDoc ri)) then Nothing else Just ("tags" .= rdTags (riDoc ri))
                            , fmap ("operationId" .=) (rdOperationId (riDoc ri))
                            , if rdDeprecated (riDoc ri) then Just ("deprecated" .= True) else Nothing
                            , if null allParams then Nothing else Just ("parameters" .= map paramDocToJson allParams)
                            , Just ("responses" .= A.object [ "200" .= A.object [ "description" .= ("Successful Response" :: T.Text) ] ])
                            ]
                    in (pathStr, m, A.object fields)
            in map mkOp methods

        allOps = concatMap routeToOps userRoutes

        -- Group by path, then by HTTP method (preserving distinct operations)
        grouped = Map.fromListWith Map.union
            [ (p, Map.singleton m op) | (p, m, op) <- allOps ]

        pathsValue = A.object
            [ Key.fromText p .= A.object [ Key.fromText m .= op | (m, op) <- Map.toList methodMap ]
            | (p, methodMap) <- Map.toList grouped
            ]

        infoFields = catMaybes
            [ Just ("title" .= docsTitle cfg)
            , Just ("version" .= docsVersion cfg)
            , fmap ("description" .=) (docsDescription cfg)
            ]
    in A.object
        [ "openapi" .= ("3.0.3" :: T.Text)
        , "info" .= A.object infoFields
        , "paths" .= pathsValue
        ]

-- | Generate the self-contained HTML page loading Swagger UI 5 from CDN.
swaggerUiHtml :: DocsConfig -> LBS8.ByteString
swaggerUiHtml DocsConfig{..} =
    LBS8.fromStrict $ TE.encodeUtf8 $ T.concat
        [ "<!DOCTYPE html>\n"
        , "<html lang=\"en\">\n"
        , "<head>\n"
        , "  <meta charset=\"UTF-8\">\n"
        , "  <title>", docsTitle, " - Swagger UI</title>\n"
        , "  <link rel=\"stylesheet\" type=\"text/css\" href=\"", docsSwaggerCssUrl, "\">\n"
        , "  <style>\n"
        , "    html { box-sizing: border-box; overflow: -moz-scrollbars-vertical; overflow-y: scroll; }\n"
        , "    *, *:before, *:after { box-sizing: inherit; }\n"
        , "    body { margin: 0; background: #fafafa; }\n"
        , "    .swagger-ui .topbar { display: none; }\n"
        , "  </style>\n"
        , "</head>\n"
        , "<body>\n"
        , "  <div id=\"swagger-ui\"></div>\n"
        , "  <script src=\"", docsSwaggerJsUrl, "\"></script>\n"
        , "  <script>\n"
        , "    window.onload = function() {\n"
        , "      window.ui = SwaggerUIBundle({\n"
        , "        url: '", openApiPath, "',\n"
        , "        dom_id: '#swagger-ui',\n"
        , "        deepLinking: true,\n"
        , "        presets: [\n"
        , "          SwaggerUIBundle.presets.apis,\n"
        , "          SwaggerUIBundle.SwaggerUIStandalonePreset\n"
        , "        ],\n"
        , "        layout: 'BaseLayout'\n"
        , "      });\n"
        , "    };\n"
        , "  </script>\n"
        , "</body>\n"
        , "</html>\n"
        ]

-- | Fallback application that intercepts requests to 'docsPath' and 'openApiPath'.
-- If the request matches, it serves Swagger UI or OpenAPI JSON; otherwise it delegates
-- to the next fallback application (typically 404).
docApp :: Monad m => DocsConfig -> [RouteInfo] -> Application m -> Application m
docApp cfg rDocs nextApp req =
    let reqPath = "/" <> T.intercalate "/" (W.pathInfo req)
        isGet = W.requestMethod req == "GET"
        isDocs = reqPath == cleanPath (docsPath cfg)
        isOpenApi = reqPath == cleanPath (openApiPath cfg)
    in if isGet && isDocs
       then return $ W.responseLBS status200 [("Content-Type", "text/html; charset=utf-8")]
                       (swaggerUiHtml cfg)
       else if isGet && isOpenApi
       then return $ W.responseLBS status200 [("Content-Type", "application/json; charset=utf-8")]
                       (A.encode (openApiJson cfg rDocs))
       else nextApp req
