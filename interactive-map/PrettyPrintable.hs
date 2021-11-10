{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module PrettyPrintable where

import qualified Diagrams.Prelude             as D
import qualified Diagrams.Backend.SVG         as D

import Utils

type Point = (Int, Int)
type Rect = (Point, Point)
type Map = [(String, String)]

class (PrettyPrintable t) => Mappable t where
  prettyPrintWithMap :: t -> (D.Diagram D.SVG, Map)

class PrettyPrintable t where
  prettyPrint :: t -> D.Diagram D.SVG

instance PrettyPrintable String where
  prettyPrint t = D.text t <> D.rect (0.8 * l) 1.2
    where l = debug $ fromIntegral $ length t

-- instance (Num a, Show a) => PrettyPrintable a where
instance PrettyPrintable Int where
  prettyPrint = prettyPrint . show

--   prettyPrintCompact = T.pack . show
--   prettyProcessCompact = T.lines . T.pack . show

-- instance PrettyPrintable a => PrettyPrintable [a] where
--     prettyPrint = prettyPrintList

-- instance PrettyPrintable a => PrettyPrintable (BinaryTree a ) where
--     prettyPrint = prettyPrintTree

-- prettyPrintTree :: PrettyPrintable a => BinaryTree a -> Picture
-- prettyPrintTree Leave = Blank
-- prettyPrintTree (Branch a l r) = Blank

-- prettyPrintList :: PrettyPrintable a => [a] -> Picture
-- prettyPrintList [] = Blank
-- prettyPrintList (x:xs) = Blank
