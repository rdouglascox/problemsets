{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE OverloadedStrings         #-}

module Printing.RenderSVG (renderTree) where

import Diagrams.Prelude
import Diagrams.Backend.SVG

import Printing.RenderPTree

import Graphics.Svg.Core
import qualified Trees.PLtrees as P
import System.Random
import Data.Text.Lazy
import qualified Graphics.SVGFonts as SF

renderme n x = renderText $ renderDia SVG (SVGOptions (mkWidth (n * 25)) Nothing "" [] True) x

renderTree :: P.Tree [P.TProp] -> IO Text
renderTree t = do
    font <- SF.loadFont "/home/STIX.svg" -- change this on the server
--    font <- SF.loadFont "./STIX.svg"
    gen <- newStdGen 
    let tree = renderPTree font t
    return (renderme (width tree) tree)
