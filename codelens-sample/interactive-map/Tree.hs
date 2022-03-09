{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}


module Tree where

import Data.Maybe
import Data.Tree
import Control.Monad (liftM, ap)
import Debug.Trace

import Diagrams.Prelude ((#), (.~), (&), (~~))
import qualified Diagrams.Prelude           as D
import qualified Diagrams.Backend.SVG       as D
import qualified Diagrams.TwoD.Layout.Tree  as D

import Crud
import Displayable
import Utils

-- instance {-# OVERLAPPING #-} Show a => Show (Tree a) where
--   show (Node x sub) = "Node " ++ show x ++ show sub

instance Displayable Tree where
  display = prettyPrintTree
  generate = makeTree

instance Editable Tree where
  editAtKey = editTreeAtKey

editTreeAtKey :: Crud -> [Key] -> Maybe a -> Tree (Key, a) -> Tree a
editTreeAtKey op [k] mv = go
  where
    go (Node (i, x) sub)
      | i == k    = if isNothing mv then Node x [] else Node (fromJust mv) (map (fmap snd) sub)
      | otherwise = Node x (map go sub)

prettyPrintTree :: (Show a) => Tree (Info, a) ->  D.Diagram D.SVG
prettyPrintTree tree = D.renderTree ((<> D.circle 1 # D.fc D.white) . r) (~~) posAnnTree
  where
    r (k, (c, e)) = D.svgId (show k) $ D.svgClass ("id" ++ show k) (showDisplay c e)
    posAnnTree = D.symmLayout' (D.with & D.slHSep .~ 4 & D.slVSep .~ 4) (annotate tree)

makeTree :: Int -> Tree (Info, Label)
makeTree 0 = undefined
