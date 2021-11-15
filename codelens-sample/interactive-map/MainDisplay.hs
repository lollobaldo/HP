{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B

import qualified Demo as File
import Displayable
import List
import Utils ( injectSvgToFile )

-- import qualified Data.ByteString.Lazy
-- import qualified Data.Text as T

main = do
    template <- B.readFile "out.template.html"
    injectSvgToFile "out1.html" template (prettyPrintWithMap File.l4)
