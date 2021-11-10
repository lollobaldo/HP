{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module PrettyPrintable where

import Debug.Trace
import Graphics.Gloss


import Tree
-- import Utils
import Graphics.Gloss.Interface.Pure.Display (Picture(Blank))

debug t = trace (show t) t

class PrettyPrintable t where
  prettyPrint :: t -> Picture
  -- prettyPrintCompact :: t -> Picture
  -- prettyProcessCompact :: t -> Picture

-- instance PrettyPrintable String where
--   prettyPrint = T.pack . show
--   prettyPrintCompact = T.pack . show
--   prettyProcessCompact = T.lines . T.pack . show

instance PrettyPrintable a => PrettyPrintable [a] where
    prettyPrint = prettyPrintList

-- instance PrettyPrintable a => PrettyPrintable (BinaryTree a ) where
--     prettyPrint = prettyPrintTree

prettyPrintTree :: Show a => BinaryTree a -> Picture
prettyPrintTree Leave = Blank
prettyPrintTree (Node a l r) = Blank

prettyPrintList :: Show a => [a] -> Picture
prettyPrintList [] = Blank
prettyPrintList (x:xs) = 
