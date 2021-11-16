{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module List where

import Data.Maybe

import Diagrams.Prelude ((#), (.~), (&), (~~))
import qualified Diagrams.Prelude             as D
import qualified Diagrams.Backend.SVG         as D

import Displayable
import Utils

instance {-# OVERLAPPABLE #-} (Displayable a, Show a) => Displayable [a] where
  prettyPrint = fst . prettyPrintListWithMap 0
  -- prettyPrintWithMap = prettyPrintListWithMap 0

-- instance {-# OVERLAPPABLE #-} (Displayable a, Show a) => Mappable [a] where
--   prettyPrintWithMap = prettyPrintListWithMap 0

instance Editable [] where
  editAtKey l k mv = snd $ foldr go (0, mempty) l
    where
      -- go :: Monoid (t a) => a -> (Key, t a) -> (Key, t a)
      -- go _ []     = error "Key not found"
      go x (i, xs)
        | i == k    = (i+1, maybe mempty return mv <> xs)
        | otherwise = (i+1, return x <> xs)

prettyPrintListWithMap :: (Displayable a, Show a) => Int -> [a] ->  (D.Diagram D.SVG, Map)
prettyPrintListWithMap _ [] = (mempty, mempty)
prettyPrintListWithMap n (x:xs) = (D.hsep 2 [e, next]
    # D.connectOutside' (D.with & D.gaps .~ D.small & D.headLength .~ D.local 0.15) idd ide, (idd, ""):ids)
    where
        (next, ids) = prettyPrintListWithMap (n+1) xs
        e = D.svgId (show n) $ D.svgClass idd (prettyPrint x # D.named idd)
        idd = "id" ++ show n
        ide = "id" ++ show (n + 1)
