{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Custom where

import Diagrams.Prelude ((#), (.~), (&), (~~))
import qualified Diagrams.Prelude           as D
import qualified Diagrams.Backend.SVG       as D
import qualified Diagrams.TwoD.Layout.Tree  as D

import Displayable

data Node a = Node a [a] deriving (Eq, Ord, Show, Read)
data Graph a = Graph [Node a] deriving (Eq, Ord, Show, Read)

instance Functor Node where fmap f (Node n ns) = Node (f n) (fmap f ns)
instance Foldable Node where foldr f z (Node n ns) = foldr f z ns
deriving instance Traversable (Node)

instance Functor Graph where fmap f (Graph ns) = Graph (fmap (fmap f) ns)
instance Foldable Graph where foldr f z g = foldr f z (vertices g)
deriving instance Traversable (Graph)

labelOf :: Node a -> a
labelOf (Node a _) = a

vertices :: Graph a -> [a]
vertices (Graph ns) = map (\(Node a _) -> a) ns

edges :: Graph a -> [(a, a)]
edges (Graph ns) = concatMap (\(Node a e) -> map (a,) e) ns


g1 :: Graph Char
g1 = Graph [Node 'a' ['b'..'d'], Node 'b' ['d'], Node 'c' ['a'..'b'], Node 'd' [], Node 'e' []]

instance {-# OVERLAPPING #-} (Displayable a, Show a, D.IsName a) => Displayable (Graph a) where
  prettyPrint = prettyPrintGraph

instance Editable Graph where
  editAtKey = editGraphAtKey

editGraphAtKey :: Graph (Key, a) -> Key -> Maybe a -> Graph a
editGraphAtKey graph k mv = graph
editGraphAtKey (Graph ns) k mv = go ns
  where
    go (Graph ns)
      | i == k    = if isNothing mv then Node x [] else Node (fromJust mv) (map (fmap snd) sub)
      | otherwise = Node x (map go sub)

prettyPrintGraph :: (Displayable a, Show a, D.IsName a) => Graph a ->  D.Diagram D.SVG
prettyPrintGraph g' = D.applyAll arrowsFactory layout
  where
    opt = (D.with & D.gaps .~ D.small & D.headLength .~ D.local 0.15 )
    g = annotate g'
    n = length $ vs
    vs = vertices g
    arrowsFactory :: [(D.Diagram D.SVG -> D.Diagram D.SVG)]
    arrowsFactory = map (uncurry (D.connectOutside' opt)) (edges g')
    layout = D.atPoints (D.trailVertices $ D.regPoly n 1) (map node vs)
    node (k, e) = D.svgId (show k) $ D.svgClass ("id" ++ show k) $ D.text (show e) # D.fontSizeL 0.2 <> D.circle 0.2 # D.named e
