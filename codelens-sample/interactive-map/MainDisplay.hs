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
    template <- B.readFile "templates/out.template.html"
    let obj = File.g1
    print obj
    injectSvgToFile "out/out1.html" template (prettyPrintWithMap obj)
