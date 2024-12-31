{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{- |
Module      : Web.Scotty.Cookie
Copyright   : (c) 2014, 2015 Mārtiņš Mačs,
              (c) 2023 Marco Zocca

License     : BSD-3-Clause
Maintainer  :
Stability   : experimental
Portability : GHC

This module provides session management functionality for Scotty web applications.

==Example usage:

@
\{\-\# LANGUAGE OverloadedStrings \#\-\}

import Web.Scotty
import Web.Scotty.Session
import Control.Monad.IO.Class (liftIO)
main :: IO ()
main = do
    -- Create a session jar
    sessionJar <- createSessionJar
    scotty 3000 $ do
        -- Route to create a session
        get "/create" $ do
            sess <- createUserSession sessionJar "user data"
            html $ "Session created with ID: " <> sessId sess
        -- Route to read a session
        get "/read" $ do
            mSession <- getUserSession sessionJar
            case mSession of
                Left _-> html "No session found or session expired."
                Right sess -> html $ "Session content: " <> sessContent sess
@
-}
module Web.Scotty.Session (
    Session (..),
    SessionId,
    SessionJar,
    SessionStatus,

    -- * Create Session Jar
    createSessionJar,

    -- * Create session
    createUserSession,
    createSession,

    -- * Read session
    readUserSession,
    readSession,
    getUserSession,
    getSession,

    -- * Add session
    addSession,

    -- * Delte session
    deleteSession,

    -- * Helper functions
    maintainSessions,
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import System.Random (randomRIO)
import Web.Scotty.Action (ActionT)
import Web.Scotty.Cookie

-- | Type alias for session identifiers.
type SessionId = T.Text

-- | Status of a session lookup.
data SessionStatus = SessionNotFound | SessionExpired
  deriving (Show, Eq)

-- | Represents a session containing an ID, expiration time, and content.
data Session a = Session
    { sessId :: SessionId
    -- ^ Unique identifier for the session.
    , sessExpiresAt :: UTCTime
    -- ^ Expiration time of the session.
    , sessContent :: a
    -- ^ Content stored in the session.
    }
    deriving (Eq, Show)

-- | Type for session storage, a transactional variable containing a map of session IDs to sessions.
type SessionJar a = TVar (HM.HashMap SessionId (Session a))

-- | Creates a new session jar and starts a background thread to maintain it.
createSessionJar :: IO (SessionJar a)
createSessionJar = do
    storage <- newTVarIO HM.empty
    _ <- forkIO $ maintainSessions storage
    return storage

-- | Continuously removes expired sessions from the session jar.
maintainSessions :: SessionJar a -> IO ()
maintainSessions sessionJar =
    forever $ do
        now <- getCurrentTime
        let stillValid sess = sessExpiresAt sess > now
        atomically $ modifyTVar sessionJar $ \m -> HM.filter stillValid m
        threadDelay 1000000
        

-- | Adds a new session to the session jar.
addSession :: SessionJar a -> Session a -> IO ()
addSession sessionJar sess =
    atomically $ modifyTVar sessionJar $ \m -> HM.insert (sessId sess) sess m

-- | Retrieves a session by its ID from the session jar.
getSession :: (MonadIO m) => SessionJar a -> SessionId -> ActionT m (Either SessionStatus (Session a))
getSession sessionJar sId =
    do
        s <- liftIO $ readTVarIO sessionJar
        case HM.lookup sId s of
          Nothing -> pure $ Left SessionNotFound
          Just sess -> do 
            now <- liftIO getCurrentTime
            if sessExpiresAt sess < now
              then deleteSession sessionJar (sessId sess) >> pure (Left SessionExpired)
              else pure $ Right sess

-- | Deletes a session by its ID from the session jar.
deleteSession :: (MonadIO m) => SessionJar a -> SessionId -> ActionT m ()
deleteSession sessionJar sId =
    liftIO $
        atomically $
            modifyTVar sessionJar $
                HM.delete sId

{- | Retrieves the current user's session based on the "sess_id" cookie.
| Returns 'Nothing' if the session is expired or does not exist.
-}
getUserSession :: (MonadIO m) => SessionJar a -> ActionT m (Either SessionStatus (Session a))
getUserSession sessionJar = do
    getCookie "sess_id" >>= \case
        Nothing -> pure $ Left SessionNotFound
        Just sid -> lookupSession sid
  where
    lookupSession = getSession sessionJar

-- | Reads the content of a session by its ID.
readSession :: (MonadIO m) => SessionJar a -> SessionId -> ActionT m (Either SessionStatus a)
readSession sessionJar sId = do
    res <- getSession sessionJar sId
    return $ sessContent <$> res

-- | Reads the content of the current user's session.
readUserSession :: (MonadIO m) => SessionJar a -> ActionT m (Either SessionStatus a)
readUserSession sessionJar = do
    res <- getUserSession sessionJar
    return $ sessContent <$> res

-- | The time-to-live for sessions, in seconds.
sessionTTL :: NominalDiffTime
sessionTTL = 36000 -- in seconds

-- | Creates a new session for a user, storing the content and setting a cookie.
createUserSession :: (MonadIO m) => SessionJar a -> a -> ActionT m (Session a)
createUserSession sessionJar content = do
    sess <- liftIO $ createSession sessionJar content
    setSimpleCookie "sess_id" (sessId sess)
    return sess

-- | Creates a new session with a generated ID, sets its expiration, and adds it to the session jar.
createSession :: SessionJar a -> a -> IO (Session a)
createSession sessionJar content = do
    sId <- liftIO $ T.pack <$> replicateM 32 (randomRIO ('a', 'z'))
    now <- getCurrentTime
    let expiresAt = addUTCTime sessionTTL now
        sess = Session sId expiresAt content
    liftIO $ addSession sessionJar sess
    return $ Session sId expiresAt content
