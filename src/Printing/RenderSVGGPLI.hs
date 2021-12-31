{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE OverloadedStrings         #-}

module Printing.RenderSVGGPLI (renderTree) where

import Diagrams.Prelude
import Diagrams.Backend.SVG

import Printing.RenderPTreeGPLI

import Graphics.Svg.Core
import Data.GPLITree
import System.Random
import Data.Text.Lazy
import qualified Graphics.SVGFonts as SF

renderme n x = renderText $ renderDia SVG (SVGOptions (mkWidth (n * 25)) Nothing "" [] True) x

renderTree :: PTree -> IO Text
renderTree t = do
    font <- SF.loadFont "/home/STIX.svg" -- change this on the server
--    font <- SF.loadFont "./STIX.svg"
    gen <- newStdGen 
    let tree = renderPTree font t
    return (renderme (width tree) tree)
