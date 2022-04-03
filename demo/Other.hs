module Other where

import Displayable

import Data.Tree

import qualified Data.ByteString.Lazy
import qualified Data.Text as T

a = 10

l1, l2, l3 :: [Int]
l1 = [1,3]
l2 = [3,0,9,3,6]

l3 = [1,2,3]

ss = ['a'..'d']

tree1 :: Tree Int
tree1 = Node {rootLabel =1, subForest = [Node {rootLabel = 2, subForest = [Node {rootLabel = 4, subForest = []},Node {rootLabel = 5, subForest = []}]},Node {rootLabel =6, subForest = [Node {rootLabel= 3, subForest =[Node {rootLabel= 50, subForest= []},Node {rootLabel = 51, subForest = []},Node {rootLabel= 52, subForest= []},Node {rootLabel = 53, subForest = []}]}]}]}
  where lf x = Node x []

sample_tree = Node 1 [Node 2 [Node 4 [], Node 5 []], Node 6 [Node 3 [Node 50 [], Node 51 [], Node 52 [], Node 53 []]]]


sample_tree2 =
  Node 1 [
    Node 2 [
      Node 4 [], Node 5 []],
    Node 6 [
      Node 3 [
        Node 50 [], Node 51 [], Node 52 [], Node 53 []]]]
