{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Utils where

import Debug.Trace
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as L
import qualified Data.ByteString.Char8  as C
import qualified Data.Text as T


import Diagrams.Prelude ((.~), (&))
import qualified Diagrams.Prelude           as D
import qualified Diagrams.TwoD.Layout.Tree  as D
import qualified Diagrams.Backend.SVG       as D
import qualified Graphics.Svg.Core

debug x = trace (show x) x

-- connectOutside :: (RealFloat n, D.Renderable (D.Path D.V2 n) b, D.IsName n1, D.IsName n2) =>
--      D.ArrowOpts n -> n1 -> n2 -> D.QDiagram b D.V2 n D.Any -> D.QDiagram b D.V2 n D.Any
-- connectOutside = D.connectOutside' (D.with & D.gaps .~ D.small & D.headLength .~ D.local 0.15)

generateHTML :: [String] -> B.ByteString -> B.ByteString
generateHTML ids svg = svg <> generateCallbacks ids

generateCallbacks :: [String] -> B.ByteString
generateCallbacks ids = ""
    <> "<script>\n"
    <> "    const ids = " <> C.pack (show ids) <> ";\n"
    <> "</script>"

printSvgToByte path svg =
    let options = D.SVGOptions (D.mkWidth 1000) Nothing (T.pack "") [] True
        svgDoc = D.renderDia D.SVG options svg
    in
        L.toStrict $ Graphics.Svg.Core.renderBS svgDoc

-- printSvgToFile path svg =
--     let bs = printSvgToByte path svg
--     in
--         B.writeFile path bs

-- printMappedSvgToFile path (svg, mp) =
--     let bs = printSvgToByte path svg
--         html = generateHTML (map fst mp) bs
--     in
--         B.writeFile path html

injectSvgToFile path template (svg, mp) =
    let bs = printSvgToByte path svg
        inject = generateHTML (map fst mp) bs
        pieces = B.breakSubstring "<!-- ### ADD HTML AFTER HERE ### -->" template
        html = fst pieces <> inject <> snd pieces
    in
        B.writeFile path html
