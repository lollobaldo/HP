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

-- data BinaryTree a = Leaf | Node a (BinaryTree a) (BinaryTree a) deriving (Show, Eq, Functor, Foldable, Traversable)

instance {-# OVERLAPPABLE #-} (Displayable a, Show a) => Displayable (Tree a) where
  prettyPrint = fst . prettyPrintTreeWithMap
  -- prettyPrintWithMap = prettyPrintTreeWithMap

-- instance {-# OVERLAPPABLE #-} (Displayable a, Show a) => Mappable (Tree a) where
--   prettyPrintWithMap = prettyPrintTreeWithMap

instance Editable Tree where
  editAtKey = editTreeAtKey

-- editTreeAtKey :: BinaryTree a -> Key -> Maybe a -> BinaryTree a
-- editTreeAtKey tree k mv = go 1 tree
--   where
--     -- go :: Key -> BinaryTree a -> BinaryTree a
--     go _ Leaf = error "ciao"
--     go i (Node x l r)
--       | i == k    = Node (fromJust mv) l r
--       | otherwise = Node x (go (2*i) l) (go (3*i) r)

editTreeAtKey :: Tree a -> Key -> Maybe a -> Tree a
editTreeAtKey tree k mv = go (annotate tree)
  where
    go (Node (i, x) sub)
      | i == k    = if isNothing mv then Node x [] else Node (fromJust mv) (map (fmap snd) sub)
      | otherwise = Node x (map go sub)

prettyPrintTreeWithMap :: (Displayable a, Show a) => Tree a ->  (D.Diagram D.SVG, Map)
prettyPrintTreeWithMap tree = (D.renderTree ((<> D.circle 1 # D.fc D.white) . r) (~~) posAnnTree, [])
  where
    r = \(k, e) -> D.svgId (show k) $ D.svgClass ("id" ++ show k) (prettyPrint e)
    posAnnTree = D.symmLayout' (D.with & D.slHSep .~ 4 & D.slVSep .~ 4) (annotate tree)

-- exampleSymmTree = D.renderTree ((<> D.circle 1 # D.fc D.white) . prettyPrint) (~~) posAnnTree
--   where
--     r = \(k, e) -> D.svgId (show n) $ D.svgClass idd (prettyPrint x # D.named idd)
--     posAnnTree = D.symmLayout' (D.with & D.slHSep .~ 4 & D.slVSep .~ 4) D.t1
-- --   # lw 0.03
-- --   # centerXY # pad 1.1


-- binToRose :: BinaryTree a -> T.Tree a
-- binToRose Leaf = undefined -- T.Node "Empty" []
-- binToRose (Node x Leaf Leaf) = T.Node x []
-- binToRose (Node x Leaf r) = T.Node x [binToRose r]
-- binToRose (Node x l Leaf) = T.Node x [binToRose l]
-- binToRose (Node x l r) = T.Node x [binToRose l, binToRose r]

