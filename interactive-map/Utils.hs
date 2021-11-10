{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Utils where

import Debug.Trace
import qualified Data.ByteString.Lazy  as B
import qualified Data.ByteString.Char8 as C
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
generateHTML ids svg =
    "<!DOCTYPE html><html><head></head><body>" <> svg <> "<script>" <> generateCallbacks ids <> "</script></body></html>"

generateCallbacks :: [String] -> B.ByteString
generateCallbacks ids = ""
    <> "const vscode = acquireVsCodeApi();"
    <> "const ids = " <> B.fromStrict (C.pack (show ids)) <> ";"
    <> "ids.forEach(id => {"
    <> "    const els = document.querySelectorAll('.' + id);"
    <> "    console.log(els);"
    <> "    els.forEach(el => {"
    <> "        console.log(el.id);"
    <> "        el.addEventListener('click', event => {"
    <> "            console.log(el.id);"
    <> "            vscode.postMessage({ id: el.id });"
    <> "    })});"
    <> "});"

printSvgToByte path svg =
    let options = D.SVGOptions (D.mkWidth 1000) Nothing (T.pack "") [] True
        svgDoc = D.renderDia D.SVG options svg
    in
        Graphics.Svg.Core.renderBS svgDoc

printSvgToFile path svg =
    let bs = printSvgToByte path svg
    in
        B.writeFile path bs

printMappedSvgToFile path (svg, mp) =
    let bs = printSvgToByte path svg
        html = generateHTML (map fst mp) bs
    in
        B.writeFile path html
