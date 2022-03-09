{-# LANGUAGE FlexibleInstances #-}

module Funcs where

import Data.Maybe

import Diagrams.Prelude ((#), (.~), (&), (~~))
import qualified Diagrams.Prelude             as D
import qualified Diagrams.Backend.SVG         as D

import Crud
import Displayable
import List
import Utils

-- type Label = Int
type FuncIn = (Info, Int)

rand :: [FuncIn]
rand = zip (rainbow 5) [1,2,3, 4, 5]

-- makePattern :: ([Int] -> [Int]) -> D.Diagram D.SVG
-- makePattern f = display D.black (f rand)

makePattern :: Displayable t => (t FuncIn -> t FuncIn) -> D.Diagram D.SVG
makePattern f = D.hsep 1 [display patterned, display (f patterned)] D.# D.lc lineColour
  where
    patterned = generate 5
