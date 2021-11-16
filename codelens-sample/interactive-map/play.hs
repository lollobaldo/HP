{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.State.Lazy

import Tree
import Displayable

import Data.Maybe

type Stack = [Int]

-- tick :: a -> State Int Int
-- tick a = do n <- get
--             put (n+5)
--             return a

-- process = do
--     n <- get
--     put (n+1)
--     return 

-- s = tick

traverse' :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
traverse' = traverse

t1 = traverse' print [1,2,3]

t2 = Node "Hello" (Node "Mamma" Leaf Leaf) (Node "Hi" (Node "Ciao" Leaf Leaf) (Node "Mamma" Leaf Leaf))


list = [1,2,3]

editTreeAtKey :: BinaryTree a -> Key -> Maybe a -> BinaryTree a
editTreeAtKey tree k mv = go (annotate tree)
    where
        go (_, Leaf) = error "ciao"
        go (i, Node x l r)
            | i == k    = Node (fromJust mv) l r
            | otherwise = Node x (go (2*i) l) (go (3*i) r)
