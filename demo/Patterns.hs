module Patterns where

f1 :: [a] -> [a]
f1 (x:y:zs) = y:x:zs
f1 x = x
