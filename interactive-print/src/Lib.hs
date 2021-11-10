{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Lib
    ( someFunc
    ) where

import Data.Tree
import Graphics.Gloss
import Graphics.Gloss.Export.PNG
-- import Diagrams.Prelude
-- import Diagrams.TwoD.Layout.Tree (renderTree,symmLayout',_slHSep,_slVSep)
-- import Diagrams.Backend.SVG.CmdLine
-- -- import Diagrams.Backend.Html5.CmdLine
-- import Graphics.Gloss.Export.PNG

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- node :: Int -> Diagram B
-- node n = text (show n) # fontSizeL 0.2 # fc white <> circle 0.2 # fc green

-- tournament :: Int -> Diagram B
-- tournament n = atPoints (trailVertices $ regPoly n 1) (map node [1..n])

-- example = tournament 5

exampleTree :: Tree String
exampleTree = Node "A" [Node "B" [], Node "C" []]

t1 = Node "Hello" [Node "Mamma" [], Node "Mamma" []]
t2 = Node "Hello" [Node "Mamma" [Node "Ciao" []], Node "Mamma" []]

p1 = pictures [text "Ciao", rectangleSolid 8 8]

main = do
    print "working?"
    exportPictureToPNG (300, 300) (makeColorI 255 200 50 255) "a.png" p1
