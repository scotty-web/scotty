{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

main = scotty 3000 $
  get "/" serve

serve = do
  template_ "template.html"
  tSet "loop" $ List [Value "hej1",Value "hej2"]
  tSet "map" $ fromPairs [("k",Value "v")]
  tSet "bool" (TIf True)
  tSet "test" $ Value "hej"




