{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
module Printing.RenderPTreeGPLI (renderPTree) where

import Data.GPLIprop
import qualified Data.GPLITree as P
import qualified Printing.UnicodeGPLIProps as G


import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.TwoD.Layout.Tree
import Data.Maybe
import Data.Tree
import Data.List

import qualified Graphics.SVGFonts as SF
import qualified Graphics.SVGFonts.ReadFont as SFR
import Diagrams.TwoD.Model (showOrigin)
import Diagrams.Transform (moveOriginBy)

-- |functions for rendering propositions to strings

renderProp :: Prop -> String
renderProp = G.printprop

printAProps :: [P.AProp] -> [String]
printAProps = map printAProp

printAProp :: P.AProp -> String
printAProp (P.AProp p True s) = G.printprop p ++ "✓" ++ " " ++ sort s 
printAProp (P.AProp p False s) = case p of
                                   (Universal c q) -> case s of
                                       [] -> G.printprop p
                                       _ -> G.printprop p ++ "\\" ++ sort s
                                   _ -> G.printprop p ++ " " ++ sort s 

printAPropsClosed :: [P.AProp] -> [String]
printAPropsClosed xs = map printAProp xs ++ ["x"]

printAPropsOpen :: [P.AProp] -> [String]
printAPropsOpen xs = map printAProp xs ++ ["↑"]

-- | functions for rendering tree diagrams

-- | custom text function, takes a font and then returns a text rendering function

text' :: SFR.PreparedFont Double -> String -> Diagram B
text' font s
  = s
  # SF.svgText def { SF.textFont = font }
  # SF.fit_height 1
  # SF.set_envelope
  # lw none # centerXY # fc (sRGB24read "#2c3e50") # lc (sRGB24read "#2c3e50")

propDiagram' font x = text' font x # pad 1.2

-- | draw node

nodeDiagram font xs = vsep 0 (map (propDiagram' font)  xs) # center # bg white # alignT

-- | something
renderTProps' font = nodeDiagram font . printAProps
renderTPropsClosed' font = nodeDiagram font . printAPropsClosed

renderTPropsOpen' font = nodeDiagram font . printAPropsOpen

-- | render proof tree to a tree of diagrams

renderPTree' font (P.Branch xs (l, r)) = Node (renderTProps' font xs) [renderPTree' font l, renderPTree' font r]
renderPTree' font (P.Leaf xs) = Node (renderTPropsOpen' font xs) []
renderPTree' font (P.DeadLeaf xs) = Node (renderTPropsClosed' font xs) []

renderPTree'' font (P.Branch xs (l, r)) = Node (renderTProps' font xs) [Node (text "" # moveOriginBy 0.99) [renderPTree'' font l, renderPTree'' font r]]
renderPTree'' font (P.Leaf xs) = Node (renderTPropsOpen' font xs) []
renderPTree'' font (P.DeadLeaf xs) = Node (renderTPropsClosed' font xs) []

-- | render tree of diagrams to tree diagram

renderPTree font t = renderTree id
             (~~)
             (symmLayout' (with & slWidth  .~ fromMaybe (0,0) . extentX
                     & slHeight .~ fromMaybe (0,0) . extentY) (renderPTree'' font t))
  # centerXY # pad 1.1 # lw 1