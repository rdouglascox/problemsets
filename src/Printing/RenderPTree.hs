{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
module Printing.RenderPTree (renderPTree) where

import Data.PLprop
import qualified Trees.PLtrees as P
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.TwoD.Layout.Tree
import Data.Maybe
import Data.Tree

import qualified Graphics.SVGFonts as SF
import qualified Graphics.SVGFonts.ReadFont as SFR
import Diagrams.TwoD.Model (showOrigin)
import Diagrams.Transform (moveOriginBy)

-- |functions for rendering propositions to strings

renderProp :: Prop -> String
renderProp (Basic x) = x
renderProp (Negation x) = "¬" ++ renderProp x
renderProp (Conjunction l r) = "(" ++ renderProp l ++ "∧" ++ renderProp r ++ ")"
renderProp (Disjunction l r) = "(" ++ renderProp l ++ "∨" ++ renderProp r ++ ")"
renderProp (Conditional l r) = "(" ++ renderProp l ++ "→" ++ renderProp r ++ ")"
renderProp (Biconditional l r) = "(" ++ renderProp l ++ "↔" ++ renderProp r ++ ")"

renderTProp :: P.TProp -> String
renderTProp (p, True) = renderProp p ++ "✓"
renderTProp (p, False) = renderProp p

renderTProps :: [P.TProp] -> [String]
renderTProps = map renderTProp

renderTPropsClosed :: [P.TProp] -> [String]
renderTPropsClosed xs = map renderTProp xs ++ ["x"]

-- | functions for rendering tree diagrams

-- | custom text function, takes a font and then returns a text rendering function

text' :: SFR.PreparedFont Double -> String -> Diagram B
text' font s
  = s
  # SF.svgText def { SF.textFont = font }
  # SF.fit_height 1
  # SF.set_envelope
  # lw none # fc black # centerXY

propDiagram' font x = text' font x # pad 1.2

-- | draw node

nodeDiagram font xs = vsep 0 (map (propDiagram' font)  xs) # center # bg white # alignT

-- | something
renderTProps' font = nodeDiagram font . renderTProps
renderTPropsClosed' font = nodeDiagram font . renderTPropsClosed

-- | render proof tree to a tree of diagrams

renderPTree' font (P.Branch xs (l, r)) = Node (renderTProps' font xs) [renderPTree' font l, renderPTree' font r]
renderPTree' font (P.Leaf xs) = Node (renderTProps' font xs) []
renderPTree' font (P.DeadLeaf xs) = Node (renderTPropsClosed' font xs) []

renderPTree'' font (P.Branch xs (l, r)) = Node (renderTProps' font xs) [Node (text "" # moveOriginBy 0.99) [renderPTree'' font l, renderPTree'' font r]]
renderPTree'' font (P.Leaf xs) = Node (renderTProps' font xs) []
renderPTree'' font (P.DeadLeaf xs) = Node (renderTPropsClosed' font xs) []

-- | render tree of diagrams to tree diagram

renderPTree font t = renderTree id
             (~~)
             (symmLayout' (with & slWidth  .~ fromMaybe (0,0) . extentX
                     & slHeight .~ fromMaybe (0,0) . extentY) (renderPTree'' font t))
  # centerXY # pad 1.1 # lw thin