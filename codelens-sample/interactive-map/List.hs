{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module List where

import Data.Maybe

import Diagrams.Prelude ((#), (.~), (&), (~~))
import qualified Diagrams.Prelude             as D
import qualified Diagrams.Backend.SVG         as D

import Crud
import Displayable
import Utils

instance Displayable [] where
  display = prettyPrintList . annotate

instance Editable [] where
  editAtKey Create = appendToStart
  editAtKey Update = editListAtKey
  editAtKey Delete = editListAtKey

prettyPrintList :: (Show a) => [(Key, (Info, a))] -> D.Diagram D.SVG
prettyPrintList [] = mempty
prettyPrintList ((n, (color, x)):xs) = D.hsep 2 [e, next]
    # D.connectOutside' (D.with & D.gaps .~ D.small & D.headLength .~ D.local 0.15) idd ide
    where
        next = prettyPrintList xs
        e = D.svgTitle (show x) $ D.svgId (show n) $ D.svgClass idd (showDisplay color x # D.named idd)
        idd = "id" ++ show n
        ide = "id" ++ show (n + 1)


appendToStart :: [Key] -> Maybe a -> [(Key, a)] -> [a]
appendToStart _ (Just a) as = a : [a | (_, a) <- as]

editListAtKey :: [Key] -> Maybe a -> [(Key, a)] -> [a]
editListAtKey [k] mv = go
  where
      go ((i, x):xs)
        | i == k    = maybe mempty return mv <> map snd xs
        | otherwise = return x <> go xs
