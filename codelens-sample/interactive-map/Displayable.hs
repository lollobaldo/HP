{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}

module Displayable where

import qualified Diagrams.Prelude             as D
import qualified Diagrams.Backend.SVG         as D

import Utils

type Point = (Int, Int)
type Rect = (Point, Point)
type Map = [(String, String)]

type Key = Int

class (Foldable t) => Editable t where
  editAtKey :: t a -> Key -> a -> t a

-- editAtKey :: (Foldable t, Functor t) => t a -> Key -> a -> t a
-- editAtKey l k v = snd $ foldr go (0, mempty) l
--   where
--     go :: Monoid (t a) => a -> (Key, t a) -> (Key, t a)
--     go c (i, acc)
--       | i == k = (i+1, return v <> acc)
--       | otherwise = (i+1, return c <> acc)

class (Displayable t) => Mappable t where
  prettyPrintWithMap :: t -> (D.Diagram D.SVG, Map)

class Displayable t where
  prettyPrint :: t -> D.Diagram D.SVG

instance {-# OVERLAPPING  #-} Displayable String where
  prettyPrint t = D.text t' <> D.rect (0.8 * l) 1.2
    where
      t' = show t
      l = debug $ fromIntegral $ length t'

instance {-# OVERLAPPABLE  #-} (Num a, Show a) => Displayable a where
  prettyPrint t = D.text t' <> D.rect (0.8 * l) 1.2
    where
      t' = show t
      l = debug $ fromIntegral $ length t'

instance {-# OVERLAPPING  #-} Mappable String where
  prettyPrintWithMap t = (prettyPrint t, [])

instance {-# OVERLAPPABLE  #-} (Num a, Show a) => Mappable a where
  prettyPrintWithMap t = (prettyPrint t, [])
