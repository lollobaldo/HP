module Demo where

-- import PrettyPrintable
-- import List
-- import Utils

import Data.Tree

import qualified Data.ByteString.Lazy
import qualified Data.Text as T

l1, l2, l3 :: [Int]
l1 = []
l2 = [1..100]
l3 = [9,3,6]

l4 = [1,3]

l5 :: [String]
l5=["1", "2"]


ss = "ciao mamma"

h = ['a', 'b']


tree1 :: Tree Int
tree1 = Node 1 [Node 2 (map lf [1..3]), Node 6 [Node 3 (map lf [50..53])]]
  where lf x = Node x []
