module Demo where

-- import PrettyPrintable
-- import List
-- import Utils

import Data.Tree

import qualified Data.ByteString.Lazy
import qualified Data.Text as T

l1, l2, l3 :: [Int]
l1 = []
l2 = [1..10]
l3 = [1,3]

l4 = l2 ++ l3

l5 :: [String]
l5=["1", "2"]


ss = "ciao mamma"

h = ['a', 'b']


tree1 :: Tree Int
tree1 = Node {rootLabel =1, subForest = [Node {rootLabel = 2, subForest = []},Node {rootLabel =6, subForest = [Node {rootLabel= 3, subForest =[Node {rootLabel= 50, subForest= []},Node {rootLabel = 51, subForest = []},Node {rootLabel= 52, subForest= []},Node {rootLabel = 53, subForest = []}]}]}]}
  where lf x = Node x []
