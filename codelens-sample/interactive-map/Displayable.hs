{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TupleSections #-}

module Displayable where

import Data.List
import Data.Foldable
import Control.Monad.State.Lazy

import qualified Diagrams.Prelude             as D
import qualified Diagrams.Backend.SVG         as D

import Crud
import Utils

type Point = (Int, Int)
type Rect = (Point, Point)
type Map = [(String, String)]

type Key = Int
type Info = D.Colour Double

-- data Annotated a = Recursive (a Annotated) | Just a

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
  editAtKey :: Crud -> [Key] -> Maybe a -> t (Key, a) -> t a

prettyPrintWithMap :: (Traversable t, Displayable t, Show a) => t a -> (D.Diagram D.SVG, Map)
prettyPrintWithMap t = (display processed D.# D.lc lineColour, map (\x -> ("id" ++ show x, "")) $ getKeys t)
  where
    processed = fmap (D.red, ) t

-- patternise :: (Traversable t, Displayable (t a)) => t a -> (D.Diagram D.SVG, Map)
-- patternise = display ()

class Displayable (t :: * -> *) where
  display :: Show a => t (Info, a) -> D.Diagram D.SVG
  -- display' :: Functor t => t a -> D.Diagram D.SVG
  -- display' = display . map (D.black, )

showDisplay :: Show a => Info -> a -> D.Diagram D.SVG
showDisplay color t = D.text t' <> D.rect (0.8 * fromIntegral (length t')) 1.2 D.# D.fc color
    where
      t' = show t

-- instance {-# OVERLAPPING  #-} Displayable Char where
--   display color t = showDisplay t D.# D.fc color

-- instance {-# OVERLAPPING  #-} Displayable String where
--   display color t = showDisplay t D.# D.fc color

-- instance {-# OVERLAPPABLE  #-} (Num a, Show a) => Displayable a where
--   display color t = showDisplay t D.# D.fc color
