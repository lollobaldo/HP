{-# LANGUAGE FlexibleInstances #-}
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

instance {-# OVERLAPPING  #-} PrettyPrintable String where
  prettyPrint t = D.text t' <> D.rect (0.8 * l) 1.2
    where
      t' = show t
      l = debug $ fromIntegral $ length t'

instance {-# OVERLAPPABLE  #-} (Num a, Show a) => PrettyPrintable a where
  prettyPrint t = D.text t' <> D.rect (0.8 * l) 1.2
    where
      t' = show t
      l = debug $ fromIntegral $ length t'

