{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B

import qualified Other as File
import Displayable
import List
import Tree
import Utils ( injectSvgToFile )

-- import qualified Data.ByteString.Lazy
-- import qualified Data.Text as T

main = do
    let key = 2
    let val = Nothing
    print $ editAtKey (annotate File.l2) key val
