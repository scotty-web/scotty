module Web.Scotty.Internal.Treegen where

import Text.StringLike
import qualified Text.HTML.TagSoup as T
import Data.Maybe()

type Attribute = (String,String)

data HTML = Tag String [Attribute] [HTML]
          | Text String
    deriving Show

getChildren :: HTML -> [HTML]
getChildren (Tag _ _ ch) = ch
getChildren (Text _)     = []

getName :: HTML -> String
getName (Tag n _ _) = n
getName (Text _)    = "text"

getHtmlChildren :: HTML -> [HTML]
getHtmlChildren = (filter justTags).getChildren
  where
    justTags (Tag _ _ _) = True
    justTags _ = False

setChildren :: HTML -> [HTML] -> HTML
setChildren (Tag n as _) ch = Tag n as ch
setChildren x _ = x

hasAttribute :: HTML -> String -> Bool
hasAttribute (Tag _ as _) a = or $ map ((==a).fst) as
hasAttribute _ _ = False

hasAttributeValue :: HTML -> String -> (String -> Bool) -> Bool
hasAttributeValue (Tag _ as _) a c = or $ map (\(an,av) -> an==a && c av) as
hasAttributeValue (Text _) _ _ = False

getAttributeValue :: HTML -> String -> Maybe String
getAttributeValue (Text _) _ = Nothing
getAttributeValue (Tag _ as _) a
  | length correct == 1  = Just $ snd $ head correct
  | otherwise            = Nothing
  where 
    correct = filter ((==a).fst) as

replaceTextContent :: HTML -> (String -> String) -> HTML
replaceTextContent (Text t) f = Text $ f $ filter (flip notElem "\n\r\t ") t
replaceTextContent (Tag _ _ [Text t]) f = Text $ f $ 
                                               filter (flip notElem "\n\r\t ") t
replaceTextContent (Tag n _ _) _ = error $ "Tag " ++ show n ++
                                           " had no immediate text child."

treeToSoup :: HTML -> [T.Tag String]
treeToSoup (Tag n as ch) =T.TagOpen n as:concatMap treeToSoup ch++[T.TagClose n]
treeToSoup (Text t) = [T.TagText t]

soupToTree :: StringLike str => [T.Tag str] -> [HTML]
soupToTree [] = []
soupToTree (T.TagText t:ts) = Text (toString t):soupToTree ts
soupToTree (T.TagOpen n as:ts) = 
  Tag (toString n) (map toStr as) (soupToTree ch):soupToTree rest
  where
    (ch,rest)   = takeUntil ts
    toStr (x,y) = (toString x,toString y)

soupToTree (_:ts) = soupToTree ts

takeUntil :: StringLike str => [T.Tag str] -> ([T.Tag str],[T.Tag str])
takeUntil = flip takeUntil' 0

takeUntil' :: StringLike str => [T.Tag str] -> Int -> ([T.Tag str],[T.Tag str])
takeUntil' (T.TagClose _:ts) 0    = ([],ts)
takeUntil' (T.TagClose n:ts) i    = contTake ts (T.TagClose n) (i-1)
takeUntil' (T.TagOpen n as:ts) i  = contTake ts (T.TagOpen n as) (i+1)
takeUntil' (t:ts) i               = contTake ts t i
takeUntil' [] _                   = ([],[])

contTake :: StringLike str => [T.Tag str] -> T.Tag str -> Int -> 
                              ([T.Tag str],[T.Tag str])
contTake t h i = (ts'',rest)
  where 
    (ts',rest) = takeUntil' t i
    ts'' = if ts' == [] then [h] else h:ts'

