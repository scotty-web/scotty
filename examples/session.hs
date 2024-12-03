{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Web.Scotty
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T

main :: IO ()
main = do
    sessionJar <- liftIO createSessionJar :: IO (SessionJar T.Text)
    scotty 3000 $ do
        -- Login route
        get "/login" $ do
            username <- queryParam "username" :: ActionM String
            password <- queryParam "password" :: ActionM String
            if username == "foo" && password == "bar"
                then do
                    _ <- createUserSession sessionJar "foo"
                    text "Login successful!"
                else
                    text "Invalid username or password."
        -- Dashboard route
        get "/dashboard" $ do
            mUser <- readUserSession sessionJar
            case mUser of
                Nothing -> text "Hello, user."
                Just userName -> text $ "Hello, " <> LT.fromStrict userName <> "."
        -- Logout route
        get "/logout" $ do
            deleteCookie "sess_id"
            text "Logged out successfully."