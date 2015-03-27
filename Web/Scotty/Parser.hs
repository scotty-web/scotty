module Web.Scotty.Parser(
  -- * Parser
  runScottyParse
  ) where

import Web.Scotty.Internal.Treegen
import Text.HTML.TagSoup
import Data.List (isInfixOf, elemIndex)
import Web.Scotty.Internal.Types
import qualified Data.Map.Strict as M
import Control.Monad.Reader

type TReader = Reader (M.Map String TemplateVariable) HTML

opts :: StringLike str => RenderOptions str
opts = RenderOptions id (const True) (const False)

-- | Parses the given html, compiles it, renders it back to HTML
-- and returns it.
runScottyParse :: String -> M.Map String TemplateVariable -> String
runScottyParse h t = renderTagsOptions opts $ treeToSoup $ runReader (rdr h) t
  where
    rdr = scottyParse.head.soupToTree.parseTags

-- | Parses the given HTML tree based on the given variables
-- and returns the new HTML tree.
-- Retrieve an HTML tree in this way:
-- 
-- > head $ soupToTree $ parseTags htmlString
--
scottyParse :: HTML -> TReader
scottyParse h
  | getName h == "span" &&
    hasAttributeValue h "lang" (=="scotty") = compile h
  | otherwise = cont
  where
    cont = do 
      ch <- mapM scottyParse (getChildren h)
      return $ setChildren h ch

-- | compiles a <span lang="scotty"> tag and returns the
-- new HTML node.
compile :: HTML -> TReader
compile h
  | isInfixOf "foreach" av = compForEach (sav !! 1) (sav !! 3) h
  | av == "splice"         = compSplice h
  | isInfixOf "if" av      = compIf (sav !! 1) h
  | otherwise              = fail $ "Invalid syntax: " ++ av
  where
    sav = split av ' '
    av = case getAttributeValue h "type" of 
        Nothing -> "splice"
        Just s  -> s

-- | Compiles a <span lang="scotty" type="if bool">
compIf :: String -> HTML -> TReader
compIf var h = do
  tv <- ask
  if true tv 
    then do
      chs <- mapM scottyParse (getChildren h)
      return $ Tag "span" [] chs
    else return $ Tag "span" [] []
  where
    true tv = case getMapVar (split var '.') tv of
              Nothing      -> False
              Just (TIf b) -> b
              Just x       -> error $ "If expected a condition but received "++show x

-- | compiles a <span lang="scotty">var</span>
compSplice :: HTML -> TReader
compSplice h = do
  tVar <- ask
  return $ replaceTextContent h (getVar tVar)
  where
    getVar t s = case getMapVar (split s '.') t of
          Nothing         -> ""
          Just (Value s') -> s'
          Just x          -> error $ "Splice expects variable, but received "++show x

-- | Compiles a <span lang="scotty" type="foreach var as temp">
compForEach :: String -> String -> HTML -> TReader
compForEach for as h = do
  tv <- ask
  chs <- mapM loc (vars tv)
  return $ Tag "span" [] $ chs
  where
    ach = Tag "span" [] (getChildren h) 

    loc :: TemplateVariable -> TReader
    loc v = local (M.insert as v) (scottyParse ach)

    vars :: M.Map String TemplateVariable -> [TemplateVariable]
    vars t = case getMapVar (split for '.') t of
      Nothing       -> []
      Just (List l) -> l 
      Just x        -> error $ "Foreach expects list, but received " ++ show x

-- | Splits the list on the specified element. The element itself is removed.
split :: Eq a => [a] -> a -> [[a]]
split [] _ = [[]]
split l sl = case ei of
        Nothing -> [l]
        Just i  -> take i l:split (drop (i+1) l) sl
  where 
    ei = elemIndex sl l

-- | Retrieves a nested map variable in the following form: ["key1","key2"]
getMapVar :: [String] -> M.Map String TemplateVariable -> Maybe TemplateVariable
getMapVar [] _ = error "Empty variable qualifier"
getMapVar (x:[]) m = M.lookup x m

getMapVar (q:qs) m = case M.lookup q m of
            Nothing -> Nothing
            Just (TMap m') -> getMapVar qs m'
            Just x -> error $ "expected map, got " ++ show x
