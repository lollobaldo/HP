{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveTraversable #-}

module Tree where

import Data.Maybe
-- import Data.Tree
import Control.Monad (liftM, ap)
import Debug.Trace

import Diagrams.Prelude ((#), (.~), (&))
import qualified Diagrams.Prelude             as D
import qualified Diagrams.Backend.SVG         as D

import Displayable
import Utils

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show, Eq, Functor, Foldable, Traversable)

-- instance {-# OVERLAPPABLE #-} (Displayable a, Show a) => Displayable [a] where
--   prettyPrint = fst . prettyPrintListWithMap 0

-- instance {-# OVERLAPPABLE #-} (Displayable a, Show a) => Mappable [a] where
--   prettyPrintWithMap = prettyPrintListWithMap 0

-- editAtKey' :: Show a => Tree a -> Key -> Maybe a -> Tree a
-- editAtKey' t k mv = snd $ foldl go (1, Leaf) t
--         where
--             go Leaf _ = error
--             go (Node v l r) (i, xs)
--                 | i == k    = if isNothing mv then (1, Leaf) else (1, Node (fromJust mv) l r)
--                 | otherwise = (i, Node v (go l (i*2)) (go r (i*3)))

-- instance Editable Tree where
--     editAtKey l k mv = snd $ foldl go (0, Leaf) l
--         where
--             go (i, xs) x = trace (show x) (i, xs)
                -- | i == k    = (i+1, xs <> maybe mempty pure mv)
                -- | otherwise = (i+1, xs <> pure x)

-- prettyPrintListWithMap :: (Displayable a, Show a) => Int -> [a] ->  (D.Diagram D.SVG, Map)
-- prettyPrintListWithMap _ [] = (mempty, mempty)
-- prettyPrintListWithMap n (x:xs) = (D.hsep 2 [e, next]
--     # D.connectOutside' (D.with & D.gaps .~ D.small & D.headLength .~ D.local 0.15) idd ide, (idd, ""):ids)
--     where
--         (next, ids) = prettyPrintListWithMap (n+1) xs
--         e = D.svgId (show n) $ D.svgClass idd (prettyPrint x # D.named idd)
--         idd = "id" ++ show n
--         ide = "id" ++ show (n + 1)
