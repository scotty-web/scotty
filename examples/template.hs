{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

main = scotty 3000 $
  get "/" serve

serve = do
  template_ "template.html"
  tSet "loop" $ List [Value "var1",Value "var2"]
  tSet "map" $ fromPairs [("k",Value "v")]
  tSet "bool" (TIf True)
  tSet "simple" $ Value "hi there"




