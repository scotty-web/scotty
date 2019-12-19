{-# language
    OverloadedStrings
  , GeneralizedNewtypeDeriving
  #-}

module Main (main) where

import Control.Monad
import Data.Default.Class (def)
import Data.Functor.Identity
import Data.Text (Text)
import Lucid.Base
import Lucid.Html5
import Web.Scotty
import Web.Scotty.Internal.Types
import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.State.Strict as SS
import qualified Data.ByteString.Lazy as BL

import Weigh

main :: IO ()
main = do
  mainWith $ do
    setColumns [Case,Allocated,GCs,Live,Check,Max,MaxOS]
    setFormat Markdown
    io "ScottyM Lazy" BL.putStr
      (SL.evalState (runS $ renderBST htmlScotty) def)
    io "ScottyM Strict" BL.putStr
      (SS.evalState (runScottyStrict $ renderBST htmlScottyStrict) def)
    io "Identity" BL.putStr
      (runIdentity $ renderBST htmlIdentity)

htmlTest :: Monad m => HtmlT m ()
htmlTest = replicateM_ 2 $ div_ $ do
  replicateM_ 1000 $ div_ $ do
    replicateM_ 10000 $ div_ "test"

htmlIdentity :: HtmlT Identity ()
htmlIdentity = htmlTest
{-# noinline htmlIdentity #-}

htmlScotty :: HtmlT ScottyM ()
htmlScotty = htmlTest
{-# noinline htmlScotty #-}

htmlScottyStrict :: HtmlT ScottyStrict ()
htmlScottyStrict = htmlTest
{-# noinline htmlScottyStrict #-}

newtype ScottyStrict a = ScottyStrict
  { runScottyStrict :: SS.State (ScottyState Text IO) a }
  deriving (Functor,Applicative,Monad)

