{-# LANGUAGE OverloadedStrings #-}
-- | Example demonstrating JSON mode in Scotty
--
-- By default, Scotty returns HTML error responses. Setting jsonMode = True
-- in Options causes all error responses to be returned as JSON instead.
--
-- To test:
--   cabal run scotty-json-mode
--   curl http://localhost:3000/        # HTML welcome page
--   curl http://localhost:3000/missing # 404 error as HTML
--   curl http://localhost:3000/error   # 500 error as HTML (missing path param)
--
-- To enable JSON mode, change `useJsonMode = False` to `useJsonMode = True` below
module Main (main) where

import Web.Scotty
import Data.Aeson (object, (.=))
import qualified Data.Text.Lazy as TL

-- | Toggle this to test JSON mode
useJsonMode :: Bool
useJsonMode = False

main :: IO ()
main = do
  if useJsonMode
    then do
      putStrLn "\n=== Running server with JSON mode ENABLED ==="
      putStrLn "All error responses will be returned as JSON"
    else do
      putStrLn "\n=== Running server with HTML mode (default) ==="
      putStrLn "Error responses will be returned as HTML"
  
  putStrLn "\nEndpoints to test:"
  putStrLn "  http://localhost:3000/        - Welcome page"
  putStrLn "  http://localhost:3000/missing - 404 Not Found"
  putStrLn "  http://localhost:3000/error   - 500 Internal Server Error"
  putStrLn "\nPress Ctrl+C to stop\n"
  
  scottyOpts (defaultOptions { jsonMode = useJsonMode }) $ do
    get "/" $ do
      if useJsonMode
        then json $ object 
          [ "message" .= ("Welcome! JSON mode is ON" :: TL.Text)
          , "endpoints" .= object 
              [ "notFound" .= ("/missing" :: String)
              , "error" .= ("/error" :: String)
              ]
          ]
        else html "<h1>Welcome! JSON mode is OFF (default)</h1><ul><li><a href='/missing'>404 Example</a></li><li><a href='/error'>500 Example</a></li></ul>"
    
    get "/error" $ do
      -- This will trigger a 500 error because we're trying to access a non-existent path parameter
      _ <- pathParam "nonexistent" :: ActionM TL.Text
      text "This will never be reached"
