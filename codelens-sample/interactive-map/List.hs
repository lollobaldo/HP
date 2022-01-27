{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module List where

import Data.Maybe

import Diagrams.Prelude ((#), (.~), (&), (~~))
import qualified Diagrams.Prelude             as D
import qualified Diagrams.Backend.SVG         as D

import Crud
import Displayable
import Utils

instance {-# OVERLAPPING #-} (Displayable a, Show a) => Displayable [a] where
  prettyPrint = prettyPrintList . annotate

instance Editable [] where
  editAtKey = editListAtKey

prettyPrintList :: (Displayable a, Show a) => [(Key, a)] -> D.Diagram D.SVG
prettyPrintList [] = mempty
prettyPrintList ((n, x):xs) = D.hsep 2 [e, next]
    # D.connectOutside' (D.with & D.gaps .~ D.small & D.headLength .~ D.local 0.15) idd ide
    where
        next = prettyPrintList xs
        e = D.svgTitle (show x) $ D.svgId (show n) $ D.svgClass idd (prettyPrint x # D.named idd)
        idd = "id" ++ show n
        ide = "id" ++ show (n + 1)

editListAtKey :: Crud -> [Key] -> Maybe a -> [(Key, a)] -> [a]
editListAtKey op [k] mv = go
  where
      go ((i, x):xs)
        | i == k    = maybe mempty return mv <> map snd xs
        | otherwise = return x <> go xs
