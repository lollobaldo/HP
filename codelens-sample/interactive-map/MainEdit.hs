{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B

import qualified Custom as File
import Displayable
import Graph
import List
import Tree
import Utils ( injectSvgToFile )

-- import qualified Data.ByteString.Lazy
-- import qualified Data.Text as T

main = do
    let key = 0
    let val = Just 'k'
    print $ editAtKey (annotate File.g1) key val
