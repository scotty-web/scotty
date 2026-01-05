{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Web.Scotty
import qualified Web.Scotty.Session as Session
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
    sessionJar <- liftIO Session.createSessionJar :: IO (Session.SessionJar T.Text)
    scotty 3000 $ do
        -- Login route
        get "/login" $ do
            username <- queryParam "username" :: ActionM String
            password <- queryParam "password" :: ActionM String
            if username == "foo" && password == "bar"
                then do
                    _ <- Session.createUserSession sessionJar Nothing "foo"
                    text "Login successful!"
                else
                    text "Invalid username or password."
        -- Dashboard route
        get "/dashboard" $ do
            mUser <- Session.readUserSession sessionJar
            case mUser of
                Left _ -> text "Hello, user. Please log in."
                Right userName -> text $ "Hello, " <> LT.fromStrict userName <> "."
        -- Logout route
        get "/logout" $ do
            deleteCookie "sess_id"
            text "Logged out successfully."
