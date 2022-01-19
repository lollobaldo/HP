{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Displayable where

import Data.List
import Data.Foldable
import Control.Monad.State.Lazy

import qualified Diagrams.Prelude             as D
import qualified Diagrams.Backend.SVG         as D

import Utils

type Point = (Int, Int)
type Rect = (Point, Point)
type Map = [(String, String)]

type Key = Int

annotate :: (Traversable t) => t b -> t (Key, b)
annotate t = evalState (traverse go t) 0
  where
    go :: a -> State Key (Key, a)
    go a = do n <- get
              put (n+1)
              return (n, a)

getKeys :: (Traversable t) => t b -> [Key]
getKeys t = toList $ evalState (traverse go t) 0
  where
    go :: a -> State Key Key
    go a = do n <- get
              put (n+1)
              return n

class (Foldable t) => Editable t where
  editAtKey :: t (Key, a) -> Key -> Maybe a -> t a

prettyPrintWithMap :: (Traversable t, Displayable (t a)) => t a -> (D.Diagram D.SVG, Map)
prettyPrintWithMap t = (prettyPrint t, map (\x -> ("id" ++ show x, "")) $ getKeys t)

class Displayable t where
  prettyPrint :: t -> D.Diagram D.SVG

instance {-# OVERLAPPING  #-} Displayable Char where
  prettyPrint t = D.text t' <> D.rect (0.8 * l) 1.2
    where
      t' = show t
      l = fromIntegral $ length t'

instance {-# OVERLAPPING  #-} Displayable String where
  prettyPrint t = D.text t' <> D.rect (0.8 * l) 1.2
    where
      t' = show t
      l = fromIntegral $ length t'

instance {-# OVERLAPPABLE  #-} (Num a, Show a) => Displayable a where
  prettyPrint t = D.text t' <> D.rect (0.8 * l) 1.2
    where
      t' = show t
      l = fromIntegral $ length t'
