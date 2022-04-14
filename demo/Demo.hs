module Demo where

import Data.Tree

l1, l2 :: [Int]
l1 = []

-- VISUALISE l2
l2 = [1,5,3]



l11 = [tree11, tree21]

tree11 = Node 1 [Node 0 [], Node 3 [Node 2 [], Node 4 []]]
tree21 = Node 1 [Node 0 [], Node 3 [Node 2 [], Node 4 []]]






tree55 = Node [1,2] [Node [0,1] [], Node [3] [Node [2] [], Node [3] []]]









l5 :: [String]
l5=["21", "2"]


ss = "ciao"

h = ['a', 'b']

treeComp :: Tree [Int]
treeComp = Node {rootLabel = [0,4], subForest = [Node {rootLabel = [2], subForest = []},Node {rootLabel = [6], subForest = [Node {rootLabel = [3], subForest = []}]}]}

tree2 = Node 1 [Node 0 [], Node 3 [Node 2 [], Node 4 []]]

tree1 :: Tree Int
tree1 = Node {rootLabel = 100, subForest = [Node {rootLabel = 2, subForest = [Node {rootLabel = 4, subForest = []}]},Node {rootLabel = 6, subForest = [Node {rootLabel = 3, subForest = [Node {rootLabel = 50, subForest = []},Node {rootLabel = 51, subForest = []},Node {rootLabel = 52, subForest = []},Node {rootLabel = 53, subForest = []}]}]}]}

sample_tree2 =
  Node 1 [
    Node 2 [
      Node 4 [], Node 5 []
    ],
    Node 6 [
      Node 3 [
        Node 50 [], Node 51 [], Node 52 [], Node 53 []
  ]]]