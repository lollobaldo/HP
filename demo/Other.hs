module Other where

-- import PrettyPrintable
-- import List
-- import Utils

import Data.Tree

import qualified Data.ByteString.Lazy
import qualified Data.Text as T

l1, l2, l3 :: [Int]
l1 = []
l2 = [1,2,4]
l3 = [1..10]

tree1 :: Tree Int
tree1 = Node 1 [Node 2 (map lf [1..3]), Node 6 [Node 3 (map lf [50..53])]]
  where lf x = Node x []
