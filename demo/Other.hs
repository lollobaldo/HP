module Other where

-- import PrettyPrintable
-- import List
-- import Utils

import Data.Tree

import qualified Data.ByteString.Lazy
import qualified Data.Text as T

l1, l2, l3 :: [Int]
l1 = [1, 2]
l2 = [1,7, 10]
l3 = [1..10]

tree1 :: Tree Int
tree1 = Node {rootLabel = 1, subForest = [Node {rootLabel = 2, subForest = []},Node {rootLabel = 6, subForest = [Node {rootLabel= 3, subForest = []}]}]}
  where lf x = Node x []
