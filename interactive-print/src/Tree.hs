{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Tree where

data BinaryTree a = Leave | Node a (BinaryTree a) (BinaryTree a)
