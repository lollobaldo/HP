{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Trial where

import PrettyPrintable
import Tree
import Utils

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T

randomTree :: BinaryTree Text
t1 = Node "Hello" (Leave "Mamma") (Leave "Mamma")
randomTree = Node "Hello" (Node "Hello" (Leave "Ciao") (Leave "Mamma")) (Leave "Mamma")

main = do
  print "Hello World"
