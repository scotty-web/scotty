{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
import Data.Aeson.TH (deriveJSON)

import qualified Data.Text.Lazy as T

import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.Text (renderHtml)

import Web.Scotty

-- A rather contrived example to test round-tripping JSON through Scotty
data Foo = Quux
         | Bar Int
         | Baz (Float, String)
    deriving (Eq, Show)

$(deriveJSON Prelude.id ''Foo)

main :: IO ()
main = scotty 3000 $ do
    middleware logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "static")

    get "/" $ do
        html $ wrapper $ do
            H.form ! A.id "fooform" ! method "post" ! action "#" $ do
                H.text "Select a constructor: "
                H.input ! type_ "radio" ! A.id "fooquux" ! name "con" ! value "Quux"
                H.label ! for "fooquux" $ "Quux"
                H.input ! type_ "radio" ! A.id "foobar" ! name "con" ! value "Bar"
                H.label ! for "foobar" $ "Bar"
                H.input ! type_ "radio" ! A.id "foobaz" ! name "con" ! value "Baz"
                H.label ! for "foobaz" $ "Baz"
                H.br
                H.text "Enter an int: "
                H.input ! type_ "text" ! class_ "barfields" ! name "Barint"
                H.br
                H.text "Enter a float: "
                H.input ! type_ "text" ! class_ "bazfields" ! name "Bazfloat"
                H.text "Enter a string: "
                H.input ! type_ "text" ! class_ "bazfields" ! name "Bazstring"
                H.br
                H.input ! type_ "submit"
            H.div ! A.id "foolog" $ ""

    post "/foo" $ do
        v <- jsonData
        json $ case v of
                Quux -> Quux
                Bar i -> Bar $ i + 1
                Baz (f,s) -> Baz (f + 0.5, s)

wrapper :: H.Html -> T.Text
wrapper content' = renderHtml
    $ H.html $ do
        H.header $ do
            -- the first two are libraries, the last is our custom code
            H.script ! type_ "text/javascript" ! src "jquery.js" $ ""
            H.script ! type_ "text/javascript" ! src "jquery-json.js" $ ""
            H.script ! type_ "text/javascript" ! src "json.js" $ ""
        H.body content'
