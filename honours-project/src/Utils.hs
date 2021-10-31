{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Utils where

import Data.List
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import System.Console.ANSI

import Debug.Trace

type Line = Text

lineLength = 80 :: Int

(|>) :: a -> (a -> b) -> b
x |> f = f x

padToLength :: Int -> Text -> Text
padToLength len str = T.justifyLeft (len - T.length str) ' ' str

-- topRow, midRow, botRow :: [Int] -> Text
topRow, midRow, botRow :: [Int] -> Text
-- topRow xs = tl `T.append` T.intercalate tc [T.replicate x "─" | x <- xs] `T.append` tr
topRow xs = "┌─" `T.append` T.intercalate "─┬─" [T.replicate x "─" | x <- xs] `T.append` "─┐"
midRow xs = "├─" `T.append` T.intercalate "─┼─" [T.replicate x "─" | x <- xs] `T.append` "─┤"
botRow xs = "└─" `T.append` T.intercalate "─┴─" [T.replicate x "─" | x <- xs] `T.append` "─┘"

inclusiveIntercalate :: Text -> [Text] -> Text
inclusiveIntercalate a b = a `T.append` T.intercalate a b `T.append` a

box :: Text -> Text
box a = boxRow [a]

boxRow :: [Text] -> Text
boxRow strs = T.intercalate "\n" [topRow ls, inclusiveIntercalate "│" paddedStrs, botRow ls]
    where
        ls = map T.length strs
        size = 2 + maximum ls
        paddedStrs = map ((`T.append` " ") . (" " `T.append`)) strs

boxTable :: [[Text]] -> Text
boxTable = T.unlines . map boxRow

center :: Int -> Text -> Text
center l = T.unlines . map (T.center l ' ') . T.lines

joinWith :: ([a] -> b) -> [[a]] -> [b]
joinWith f = map f . transpose

concat :: [Line] -> Text
concat = T.intercalate "\n" . joinWith T.concat . map T.lines

concat2 :: [Line] -> [Line] -> [Line]
concat2 = zipWith T.append

indentLines :: Int -> [Line] -> [Line]
indentLines n = map (T.replicate n " " `T.append`)

indent :: Int -> Text -> Text
indent n = T.unlines . indentLines n . T.lines

getColor c = T.pack $ setSGRCode [SetColor Foreground Vivid c]
getReset = T.pack $ setSGRCode [Reset]
