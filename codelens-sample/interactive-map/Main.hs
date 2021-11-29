module Main where

import qualified Custom as File

import qualified Data.ByteString.Char8 as B

import Displayable
import List
import Tree
import Utils ( injectSvgToFile )

graph :: (Displayable (t a), Traversable t, Show (t a)) => t a -> IO ()
graph a = do
    template <- B.readFile "templates/out.template.html"
    print a
    injectSvgToFile "out/out1.html" template (prettyPrintWithMap a)

edit :: (Editable t, Show (t a), Traversable t) => t a -> Key -> Maybe a -> IO ()
edit a k v = do
    print $ editAtKey (annotate a) k v

main = do
    print "Use this module to compile and call the functions."
