{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module List where

import Diagrams.Prelude ((#), (.~), (&))
import qualified Diagrams.Prelude             as D
import qualified Diagrams.Backend.SVG         as D

import PrettyPrintable
import Utils

instance (PrettyPrintable a, Show a) => PrettyPrintable [a] where
  prettyPrint = fst . prettyPrintListWithMap 0

instance (PrettyPrintable a, Show a) => Mappable [a] where
  prettyPrintWithMap = prettyPrintListWithMap 0

prettyPrintListWithMap :: (PrettyPrintable a, Show a) => Int -> [a] ->  (D.Diagram D.SVG, Map)
prettyPrintListWithMap _ [] = (mempty, mempty)
prettyPrintListWithMap n (x:xs) = (D.hsep 2 [e, next]
    # D.connectOutside' (D.with & D.gaps .~ D.small & D.headLength .~ D.local 0.15) idd ide, (idd, ""):ids)
    where
        (next, ids) = prettyPrintListWithMap (n+1) xs
        e = D.svgId (show x) $ D.svgClass idd (prettyPrint x # D.named idd)
        idd = "id" ++ show n
        ide = "id" ++ show (n + 1)
