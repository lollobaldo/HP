{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Other as File

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy   as L
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Text.Encoding
import qualified Diagrams.Prelude             as D

import Crud
import Displayable
import Funcs
import List
import Protocol
import Tree
import Utils

graph :: (Displayable t, Traversable t, Show a, Show (t a), D.IsName a) => t a -> IO ()
graph a = do
    template <- B.readFile "templates/out.template.html"
    let html = generateHtml template (prettyPrintWithMap a)
    C.putStrLn $ A.encode (GraphResponse (Info "" (0, 0)) (Right (decodeUtf8 html)))

edit :: (Editable t, Show (t a), Traversable t) => Crud -> [Key] -> Maybe a -> t a -> IO ()
edit c ks mv struct = do
    let code = show $ editAtKey c ks mv (annotate struct)
    C.putStrLn $ A.encode (EditResponse (Info "" (0, 0)) (Right code))

pattern :: Displayable t => (t FuncIn -> t FuncIn) -> IO ()
pattern f = do
    template <- B.readFile "templates/out.template.html"
    let html = generateHtml template (makePattern f, [])
    C.putStrLn $ A.encode (GraphResponse (Info "" (0, 0)) (Right (decodeUtf8 html)))

main = do
    print "Use this module to compile and call the functions."
