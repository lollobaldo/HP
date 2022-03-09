data Color = R | B deriving Show

data Tree a = E | T Color (Tree a) a (Tree a) deriving Show

