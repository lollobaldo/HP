module Other where

-- import PrettyPrintable
-- import List
-- import Utils

import Data.Tree

import qualified Data.ByteString.Lazy
import qualified Data.Text as T

l1, l2, l3 :: [Int]
l1 = [1,3]
l2 = [3,0,9,3,6]
l3 = [1..3]

ss = ['a'..'d']

tree1 :: Tree Int
tree1 = Node {rootLabel = 1, subForest = [Node {rootLabel = 2, subForest = []},Node {rootLabel = 4, subForest = []}]}
  where lf x = Node x []
