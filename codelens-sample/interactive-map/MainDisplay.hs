
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
    template <- B.readFile "out.template.html"
    let obj = File.l2
    print obj
    injectSvgToFile "out1.html" template (prettyPrintWithMap obj)
