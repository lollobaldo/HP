{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Tree where

data BinaryTree a = Leave a | Node a (BinaryTree a) (BinaryTree a)
