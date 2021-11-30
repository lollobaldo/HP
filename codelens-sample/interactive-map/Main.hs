{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Other as File

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy   as L
import Data.Text.Encoding

import Displayable
import List
import Protocol
import Tree
import Utils

graph :: (Displayable (t a), Traversable t, Show (t a)) => t a -> IO ()
graph a = do
    template <- B.readFile "templates/out.template.html"
    let html = generateHtml template (prettyPrintWithMap a)
    L.putStrLn $ A.encode (GraphResponse (Info "" (0, 0)) (Right (decodeUtf8 html)))

edit :: (Editable t, Show (t a), Traversable t) => t a -> Key -> Maybe a -> IO ()
edit a k v = do
    let code = show $ editAtKey (annotate a) k v
    L.putStrLn $ A.encode (EditResponse (Info "" (0, 0)) (Right code))


main = do
    print "Use this module to compile and call the functions."
