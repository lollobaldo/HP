{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module PrettyPrintable where

import qualified Data.Text as T
import Data.Text (Text)
import Debug.Trace
import System.Console.ANSI

import Tree
import Utils

debug t = trace (show t) t

class PrettyPrintable t where
  prettyPrint :: t -> Text
  prettyPrintCompact :: t -> Text
  prettyProcessCompact :: t -> [Text]

instance PrettyPrintable String where
  prettyPrint = T.pack . show
  prettyPrintCompact = T.pack . show
  prettyProcessCompact = T.lines . T.pack . show

instance PrettyPrintable Text where
  prettyPrint = T.pack . show
  prettyPrintCompact = T.pack . show
  prettyProcessCompact = T.lines . T.pack . show

instance PrettyPrintable a => PrettyPrintable (BinaryTree a ) where
    prettyPrint = prettyPrintTree
    prettyPrintCompact = T.unlines . prettyProcessCompactTree
    prettyProcessCompact = prettyProcessCompactTree


prettyPrintTree :: PrettyPrintable a => BinaryTree a -> Text
prettyPrintTree = h lineLength
  where
    h l (Leave v) = v |> prettyPrint |> box |> (T.replicate 1 "│\n" `T.append`) |> center l
    h l (Node v a b) = unlines' [node, join [h (l `div` 2) a,h (l `div` 2) b] ]
      where
        center' = center l
        unlines' = T.intercalate "\n"
        node = do
          let g = T.replicate 1 "│\n" `T.append` box (prettyPrint v)
          let h = (\x -> center' (unlines' (init x)) `T.append` addBranches (last x)) . T.lines $ g
          h
        addBranches :: Text -> Text
        addBranches s = T.init . center' . T.cons '┌' . flip T.snoc '┐' $ T.center ((l - T.length s + 8) `div` 2) '─' s
        join :: [Text] -> Text
        join = unlines' . joinWith T.concat . map T.lines

prettyProcessCompactTree :: PrettyPrintable a => BinaryTree a -> [Text]
prettyProcessCompactTree = h
  where
    ppc = prettyProcessCompact
    h (Leave v) = branchedValue
      where
        branchedValue = ("───" : replicate (length (ppc v) - 1) "│  ") `concat2` ppc v
    h (Node v a b) = branchedValue ++ branchedNode1 ++ branchedNode2
      where
        branchedValue = ("┬──" : replicate (length (ppc v) - 1) "│  ") `concat2` ppc v
        branchedNode1 = ("├──" : replicate (length (h a) - 1) "│  ") `concat2` h a
        branchedNode2 = ("└──" : replicate (length (h b) - 1) "   ") `concat2` h b
