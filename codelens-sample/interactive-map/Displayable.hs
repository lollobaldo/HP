{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}

module Displayable where

import Data.List

import qualified Diagrams.Prelude             as D
import qualified Diagrams.Backend.SVG         as D

import Utils

type Point = (Int, Int)
type Rect = (Point, Point)
type Map = [(String, String)]

type Key = Int

class (Foldable t) => Editable t where
  editAtKey :: t a -> Key -> Maybe a -> t a

-- label = flip evalState 0 . mapM (\a -> get >>= \i -> modify (+1) >> return (a,i))

-- editAtKey :: (Traversable t, Applicative t, Monoid (t a)) => t a -> Key -> Maybe a -> t a
-- editAtKey l k mv = snd $ foldl' go (0, mempty) l
--   where
--     go (i, xs) x
--       | i == k    = (i+1, xs <> maybe mempty pure mv)
--       | otherwise = (i+1, xs <> pure x)

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
