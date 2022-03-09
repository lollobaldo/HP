module Patterns where

import qualified Data.ByteString.Lazy
import qualified Data.Text as T

smoke = [1,2,3]

f1 :: [a] -> [a]
f1 (x:y:zs) = y:x:zs
f1 x = x
