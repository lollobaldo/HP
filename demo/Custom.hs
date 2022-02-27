{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Custom where

import Diagrams.Prelude ((#), (.~), (&), (~~))
import qualified Diagrams.Prelude           as D
import qualified Diagrams.Backend.SVG       as D
import qualified Diagrams.TwoD.Layout.Tree  as D

import Crud
import Displayable

data Node a = Node a [a] deriving (Eq, Ord, Show, Read)
data Graph a = Graph [Node a] deriving (Eq, Ord, Show, Read)

instance Functor Node where fmap f (Node n ns) = Node (f n) (fmap f ns)
instance Foldable Node where foldr f z (Node n ns) = foldr f z ns
deriving instance Traversable (Node)

instance Functor Graph where fmap f (Graph ns) = Graph (fmap (fmap f) ns)
instance Foldable Graph where foldr f z g = foldr f z (vertices g)
deriving instance Traversable (Graph)

g1 :: Graph Char
g1 = Graph [Node 'a' "cd",Node 'b' "cd",Node 'c' "a",Node 'd' "",Node 'f' "abcde",Node 'z' ""]

labelOf :: Node a -> a
labelOf (Node a _) = a

vertices :: Graph a -> [a]
vertices (Graph ns) = map (\(Node a _) -> a) ns

edges :: Graph a -> [(a, a)]
edges (Graph ns) = concatMap (\(Node a e) -> map (a,) e) ns

mapVertices :: (a -> a) -> Graph a -> Graph a
mapVertices f (Graph ns) = Graph [ Node (f i) ngs | (Node i ngs) <- ns]

mapEdges :: ([a] -> [a]) -> Graph a -> Graph a
mapEdges f (Graph ns) = Graph [ Node i (f ngs) | (Node i ngs) <- ns]

addNode :: Node a -> Graph a -> Graph a
addNode n (Graph ns) = Graph (n : ns)

deleteNode :: Eq a => a -> Graph a -> Graph a
deleteNode k (Graph ns) = Graph [ Node i (filter (/= k) ngs) | (Node i ngs) <- ns, i /= k]

deleteNodeBy :: (a -> Bool) -> Graph a -> Graph a
deleteNodeBy f (Graph ns) = Graph [ Node i (filter (not . f) ngs) | (Node i ngs) <- ns, (not . f) i]

instance {-# OVERLAPPING #-} (Displayable a, Show a, D.IsName a) => Displayable (Graph a) where
  prettyPrint = prettyPrintGraph

instance Editable Graph where
  editAtKey Create = addGraphNode
  -- editAtKey Update = editListAtKey
  editAtKey Delete = deleteGraphNode

disannotateGraph :: Graph (Key, a) -> Graph a
disannotateGraph (Graph ns) = Graph [Node a (map snd ls) |  (Node (_, a) ls) <- ns]

addGraphNode :: [Key] -> Maybe a -> Graph (Key, a) -> Graph a
addGraphNode _ (Just n) graph = addNode (Node n []) (disannotateGraph graph)

deleteGraphNode :: [Key] -> Maybe a -> Graph (Key, a) -> Graph a
-- deleteGraphNode [k] Nothing graph@(Graph ns) = mapEdges (filter (==label)) $ disannotateGraph $ deleteNodeBy f graph
deleteGraphNode [k] Nothing graph@(Graph ns) = disannotateGraph $ deleteNodeBy f graph
  where
    f = (\(i,_) -> i == k)
    the [x] = x
    label = the $ map (\(_,a) -> a) $ filter f (vertices graph)


-- editGraphAtKey :: [Key] -> Maybe a -> Graph (Key, a) -> Graph a
-- editGraphAtKey [k] mv (Graph ns) = go graph
--   where
--     go (Graph ns)
--       | i == k    = if isNothing mv then Node x [] else Node (fromJust mv) (map (fmap snd) sub)
--       | otherwise = Node x (map go sub)

prettyPrintGraph :: (Displayable a, Show a, D.IsName a) => Graph a -> D.Diagram D.SVG
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
