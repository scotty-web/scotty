-- | This module is essentially identical to 'Web.Scotty.Trans', except that 
-- some functions take/return strict Text instead of the lazy ones.
--
-- It should be noted that most of the code snippets below depend on the
-- OverloadedStrings language pragma.
--
-- The functions in this module allow an arbitrary monad to be embedded
-- in Scotty's monad transformer stack in order that Scotty be combined
-- with other DSLs.
--
-- Scotty is set up by default for development mode. For production servers,
-- you will likely want to modify 'settings' and the 'defaultHandler'. See
-- the comments on each of these functions for more information.
module Web.Scotty.Trans.Strict
    ( -- * scotty-to-WAI
      scottyT, scottyAppT, scottyOptsT, scottySocketT, Options(..), defaultOptions
      -- * Defining Middleware and Routes
      --
      -- | 'Middleware' and routes are run in the order in which they
      -- are defined. All middleware is run first, followed by the first
      -- route that matches. If no route matches, a 404 response is given.
    , middleware, get, post, put, delete, patch, options, addroute, matchAny, notFound, setMaxRequestBodySize
      -- ** Route Patterns
    , capture, regex, function, literal
      -- ** Accessing the Request, Captures, and Query Parameters
    , request, Base.header, Base.headers, body, bodyReader
    , param, params
    , captureParam, formParam, queryParam
    , captureParamMaybe, formParamMaybe, queryParamMaybe
    , captureParams, formParams, queryParams
    , jsonData, files
      -- ** Modifying the Response 
    , status, Base.addHeader, Base.setHeader
      -- ** Redirecting
    , Base.redirect, Base.redirect300, Base.redirect301, Base.redirect302, Base.redirect303
    , Base.redirect304, Base.redirect307, Base.redirect308
      -- ** Setting Response Body
      --
      -- | Note: only one of these should be present in any given route
      -- definition, as they completely replace the current 'Response' body.
    , Base.text, Base.html, file, json, stream, raw, nested
    , textLazy
    , htmlLazy
      -- ** Accessing the fields of the Response
    , getResponseHeaders, getResponseStatus, getResponseContent
      -- ** Exceptions
    , Base.raise, Base.raiseStatus, throw, rescue, next, finish, defaultHandler, liftAndCatchIO
    , StatusError(..)
    , ScottyException(..)
      -- * Parsing Parameters
    , Param, Parsable(..), readEither
      -- * Types
    , RoutePattern, File, Content(..), Kilobytes, ErrorHandler, Handler(..)
      -- * Monad Transformers
    , ScottyT, ActionT
    , ScottyState, defaultScottyState
    ) where
import Web.Scotty.Action as Base
import Web.Scotty.Trans
