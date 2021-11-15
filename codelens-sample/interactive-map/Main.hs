{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B

import qualified Demo as File
import PrettyPrintable ( Mappable(prettyPrintWithMap) )
import List
import Utils ( injectSvgToFile )

-- import qualified Data.ByteString.Lazy
-- import qualified Data.Text as T

main = do
    print "working?"
    -- print File.l2
    template <- B.readFile "out.template.html"
    -- template <- hGetContents handle
    injectSvgToFile "out1.html" template (prettyPrintWithMap File.ss)
