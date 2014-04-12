{-# LANGUAGE OverloadedStrings #-}

module SpecHelper
  ( get
  , post
  , put
  , delete
  , patch
  , request
  , body
  , status
  , header
  , headers
  ) where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Monoid          (mempty)
import           Network.HTTP.Types
import           Network.Wai          (Application, Request, requestMethod)
import           Network.Wai.Test     (SRequest (..), SResponse (..),
                                       defaultRequest, runSession, setPath,
                                       simpleBody, simpleHeaders, srequest)

-- | Send GET request to given WAI application with given path
get :: Application -> BS.ByteString -> IO SResponse
get app path = request app methodGet path mempty

-- | Send POST request to given WAI application with given path and body
post :: Application -> BS.ByteString -> LBS.ByteString -> IO SResponse
post app = request app methodPost

-- | Send PUT request to given WAI application with given path and body
put :: Application -> BS.ByteString -> LBS.ByteString -> IO SResponse
put app = request app methodPut

-- | Send DELETE request to given WAI application with given path
delete :: Application -> BS.ByteString -> IO SResponse
delete app path = request app methodDelete path mempty

-- | Send PATCH request to given WAI application with given path and body
patch :: Application -> BS.ByteString -> LBS.ByteString -> IO SResponse
patch app = request app methodPatch

-- | Return response body of given WAI reponse
body :: SResponse -> LBS.ByteString
body = simpleBody

-- | Return header of given WAI reponse
header :: HeaderName -> SResponse -> Maybe BS.ByteString
header key response = lookup key (headers response)

-- | Return all headers of given WAI reponse
headers :: SResponse -> ResponseHeaders
headers = simpleHeaders

-- | Return response status of given WAI reponse
status :: SResponse -> Status
status = simpleStatus

-- | Send request to given WAI application, with given HTTP method, path
-- and body
request :: Application -> Method -> BS.ByteString -> LBS.ByteString -> IO SResponse
request app method path requestBody =
  runSession (srequest (SRequest request' requestBody)) app
      where request' = defaultRequest
                     `setPath` path
                     `setMethod` method

-- | Set given request method to the request
setMethod :: Request -> Method -> Request
setMethod req method = req { requestMethod  = method }
