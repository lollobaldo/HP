{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Demo as File
import PrettyPrintable ( Mappable(prettyPrintWithMap) )
import Utils ( printMappedSvgToFile )

import qualified Data.ByteString.Lazy
import qualified Data.Text as T

main = do
    print "working?"
    printMappedSvgToFile "out1.html" (prettyPrintWithMap File.l2)
