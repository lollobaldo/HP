{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}

module Tree where

import Data.Maybe
import Data.Tree
import Control.Monad (liftM, ap)
import Debug.Trace

import Diagrams.Prelude ((#), (.~), (&), (~~))
import qualified Diagrams.Prelude           as D
import qualified Diagrams.Backend.SVG       as D
import qualified Diagrams.TwoD.Layout.Tree  as D

import Displayable
import Utils

instance {-# OVERLAPPABLE #-} (Displayable a, Show a) => Displayable (Tree a) where
  prettyPrint = fst . prettyPrintTreeWithMap

instance Editable Tree where
  editAtKey = editTreeAtKey

editTreeAtKey :: Tree (Key, a) -> Key -> Maybe a -> Tree a
editTreeAtKey tree k mv = go tree
  where
    go (Node (i, x) sub)
      | i == k    = if isNothing mv then Node x [] else Node (fromJust mv) (map (fmap snd) sub)
      | otherwise = Node x (map go sub)

prettyPrintTreeWithMap :: (Displayable a, Show a) => Tree a ->  (D.Diagram D.SVG, Map)
prettyPrintTreeWithMap tree = (D.renderTree ((<> D.circle 1 # D.fc D.white) . r) (~~) posAnnTree, [])
  where
    r = \(k, e) -> D.svgId (show k) $ D.svgClass ("id" ++ show k) (prettyPrint e)
    posAnnTree = D.symmLayout' (D.with & D.slHSep .~ 4 & D.slVSep .~ 4) (annotate tree)

