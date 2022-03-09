{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Custom where

import Data.List

import Diagrams.Prelude ((#), (.~), (&), (~~))
import qualified Diagrams.Prelude           as D
import qualified Diagrams.Backend.SVG       as D
import qualified Diagrams.TwoD.Layout.Tree  as D

import Crud
import Displayable
import Utils

type Edge a = (a,a)
data Node a = Node a [a] deriving (Eq, Ord, Show, Read)
data Graph a = Graph [Node a] deriving (Eq, Ord, Show, Read)

instance Functor Node where fmap f (Node n ns) = Node (f n) (fmap f ns)
instance Foldable Node where foldr f z (Node n ns) = foldr f z ns
deriving instance Traversable (Node)

instance Functor Graph where fmap f (Graph ns) = Graph (fmap (fmap f) ns)
instance Foldable Graph where foldr f z g = foldr f z (vertices g)
deriving instance Traversable (Graph)

g1 :: Graph Char
g1 = Graph [Node 'a' "cd",Node 'b' "cd",Node 'c' "af",Node 'd' "a",Node 'f' "abcde",Node 'z' ""]
g2 = Graph [Node 0 [2..5],Node 1 [0,3,4,5],Node 2 [0,1,4,5],Node 3 [0,1,2,5],Node 4 [0,1,2,3],Node 5 [1,2,3,4]]


path0to1 :: (Eq a, Show a) => Graph a -> Graph a
path0to1 graph = addEdges (zip path (tail path)) . mapEdges (\x -> []) $ graph
  where
    path = findPath zero one graph
    (zero:one:_) = vertices graph

findPath :: (Eq a, Show a) => a -> a -> Graph a -> [a]
findPath a b graph
  | b `elem` neighborsOf a graph = [a,b]
  | otherwise = a:next
  where
    next = select $ map nexts (neighborsOf a graph)
    select = shortest . filter (\x -> last x == b) . filter (not . null)
    nexts a' = findPath a' b removedA
    removedA = deleteEdgesOf a graph

shortest :: [[a]] -> [a]
shortest [] = []
shortest ls = minBy (\l -> if null l then maxBound else length l) ls

minBy :: Ord b => (a -> b) -> [a] -> a
minBy f list =
  case list of
    [x] -> x
    (x:x2:xs) -> minBy f (if f x > f x2 then x2:xs else x:xs)

getNode :: Eq a => a -> Graph a -> Node a
getNode a (Graph ns) =
  case filter (\(Node x _) -> a == x) ns of
    [x] -> x
    _ -> undefined

neighborsOf :: Eq a => a -> Graph a -> [a]
neighborsOf a graph = case getNode a graph of (Node _ as) -> as

labelOf :: Node a -> a
labelOf (Node a _) = a

vertices :: Graph a -> [a]
vertices (Graph ns) = map (\(Node a _) -> a) ns

edges :: Graph a -> [Edge a]
edges (Graph ns) = concatMap (\(Node a e) -> map (a,) e) ns

mapVertices :: (a -> a) -> Graph a -> Graph a
mapVertices f (Graph ns) = Graph [ Node (f i) ngs | (Node i ngs) <- ns]

mapEdges :: ([a] -> [a]) -> Graph a -> Graph a
mapEdges f (Graph ns) = Graph [ Node i (f ngs) | (Node i ngs) <- ns]

deleteEdgesOf :: Eq a => a -> Graph a -> Graph a
deleteEdgesOf a (Graph ns) = Graph [Node x (if x == a then [] else xs) | (Node x xs) <- ns]

addNode :: Node a -> Graph a -> Graph a
addNode n (Graph ns) = Graph (n : ns)

addEdge :: Eq a => Edge a -> Graph a -> Graph a
addEdge (a,b) (Graph ns) = Graph [Node x (if x == a then b:xs else xs) | (Node x xs) <- ns]

addEdges :: Eq a => [Edge a] -> Graph a -> Graph a
addEdges [] graph = graph
addEdges (e:es) graph = addEdges es $ addEdge e graph

deleteNode :: Eq a => a -> Graph a -> Graph a
deleteNode k (Graph ns) = Graph [ Node i (filter (/= k) ngs) | (Node i ngs) <- ns, i /= k]

deleteNodeBy :: (a -> Bool) -> Graph a -> Graph a
deleteNodeBy f (Graph ns) = Graph [ Node i (filter (not . f) ngs) | (Node i ngs) <- ns, (not . f) i]

instance Displayable Graph where
  display :: (Show a, D.IsName a) => Graph (Info, a) -> D.Diagram D.SVG
  display = prettyPrintGraph
  generate = generateGraph

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

prettyPrintGraph :: (Show a, D.IsName a) => Graph (Info, a) ->  D.Diagram D.SVG
prettyPrintGraph g' = D.applyAll arrowsFactory layout
  where
    opt = (D.with & D.gaps .~ D.small & D.headLength .~ D.local 0.15 )
    g = annotate g'
    n = length $ vs
    vs = vertices g
    arrowsFactory :: [(D.Diagram D.SVG -> D.Diagram D.SVG)]
    arrowsFactory = map (uncurry (D.connectOutside' opt)) (edges . fmap snd $ g')
    layout = D.atPoints (D.trailVertices $ D.regPoly n 1) (map node vs)
    node (k, (color, e)) = D.svgId (show k) $ D.svgClass ("id" ++ show k) $ D.text (show e) # D.fontSizeL 0.2 <> D.circle 0.2 # D.named e # D.fc color


-- [Node 'a' "cd",Node 'b' "cd",Node 'c' "a",Node 'd' "",Node 'f' "abcde",Node 'z' ""]
generateGraph :: Int -> Graph (Info, Label)
generateGraph n = fmap colorise $ Graph (map makeNode [0..n-1])
  where
    makeNode i = Node i ([0..n-1] \\ [i, (i+1) `mod` (n-1)])
    colorise i = (rainbow (n) !! i, i)
