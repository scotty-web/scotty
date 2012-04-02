{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad.IO.Class (liftIO)

import qualified Data.Map as M
import Data.Monoid
import qualified Data.Text.Lazy as T

import Network.Wai
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static

import Numeric

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.Text (renderHtml)

import Web.ClientSession
import Web.Scotty
import Web.Scotty.Util

import Debug.Trace

mkFlash :: MVar (M.Map T.Text T.Text) -> (T.Text -> ActionM (), ActionM T.Text)
mkFlash mvar = ( \t -> liftIO $ modifyMVar_ mvar $ \m -> return $ M.insert "flash" t m
               , do v <- liftIO $ modifyMVar mvar $ \m -> return (M.delete "flash" m, M.lookup "flash" m)
                    return $ maybe "" (\f -> "<span class=\"flash\">" <> f <> "</span>") v
               )

mkCookie :: IO (T.Text -> T.Text -> T.Text -> ActionM (), ActionM (Maybe T.Text))
mkCookie = do
    key <- getDefaultKey
    return (\k v e -> do bs <- liftIO $ encryptIO key $ lazyTextToStrictByteString v
                         header "Set-Cookie" $ k <> "=" <> strictByteStringToLazyText bs <> "; " <> e
           , do hs <- requestHeaders <$> request
                return $ do c <- strictByteStringToLazyText <$> lookup "Cookie" hs
                            d <- snd <$> (safeHead $ filter ((=="sid") . fst) $ map (T.breakOn "=" . T.strip) $ T.splitOn ";" c)
                            fmap strictByteStringToLazyText $ decrypt key $ lazyTextToStrictByteString $ T.tail d
           )

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

main :: IO ()
main = do
    session <- newMVar (M.empty :: M.Map T.Text T.Text)
    sIds <- newMVar (0 :: Int)
    let (flash, readFlash) = mkFlash session

    (setCookie, readCookie) <- mkCookie

    scotty 3000 $ do
        middleware logStdoutDev
        middleware $ staticRoot "static"

        get "/" $ do
            fv <- readFlash
            html $ wrapper $ do
                H.preEscapedLazyText fv
                H.form ! A.id "login" ! method "post" ! action "/login" $ do
                    H.text "Enter class key: "
                    H.input ! type_ "text" ! name "code"
                    H.br
                    H.input ! type_ "submit"

        post "/login" $ do
            c <- param "code"
            if c == ("password" :: T.Text)
            then do sId <- liftIO $ modifyMVar sIds $ \i -> return (i+1,T.pack $ showHex i "")
                    setCookie "sid" sId ""
                    redirect "/student"
            else do flash "login code incorrect!"
                    redirect "/"

        get "/logout" $ do
            setCookie "sid" "" "; expires=" -- TODO: now()
            flash "Logout successful!"
            redirect "/"

        get "/student" $ do
            sId <- maybe (do flash "not logged in!"; redirect "/") return =<< readCookie
            html $ wrapper $ do
                H.lazyText sId
                H.a ! href "/logout" $ H.text "Log out"

        get "/professor" $ do
            text "professor"

wrapper :: H.Html -> T.Text
wrapper content' = renderHtml
    $ H.html $ do
        H.header $ do
            -- the first two are libraries, the last is our custom code
            H.script ! type_ "text/javascript" ! src "jquery.js" $ ""
            H.script ! type_ "text/javascript" ! src "jquery-json.js" $ ""
            H.script ! type_ "text/javascript" ! src "clicker.js" $ ""
        H.body content'
