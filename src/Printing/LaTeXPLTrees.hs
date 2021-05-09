{-# LANGUAGE OverloadedStrings #-}


module Printing.LaTeXPLTrees (printtree) where

import Text.LaTeX.Base
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Math
import Text.LaTeX.Base.Pretty
import Text.LaTeX.Packages.Trees.Qtree
import Text.LaTeX.Packages.AMSSymb
import Printing.LaTeXPLProps
import Data.PLprop
import qualified Trees.PLtrees as L

printtree x = tree id (pltreetoLaTeX x)

-- ordinary trees

pltreetoLaTeX :: L.Tree [L.TProp] -> Tree LaTeX
pltreetoLaTeX (L.Leaf xs) = Leaf $ nodetoLaTeX $ xs 
pltreetoLaTeX (L.DeadLeaf xs) = Leaf $ deadnodetoLaTeX $ xs 
pltreetoLaTeX (L.Branch xs (l,r)) = Node (Just $ nodetoLaTeX $ xs) [pltreetoLaTeX l, pltreetoLaTeX r]

nodetoLaTeX :: [L.TProp] -> LaTeX
nodetoLaTeX [] = fromString "o" 
nodetoLaTeX ((p,True):xs) = (printprop p) <> checkmark <> lnbk <> (nodetoLaTeX xs) 
nodetoLaTeX ((p,False):xs) = (printprop p) <> lnbk <> (nodetoLaTeX xs) 

deadnodetoLaTeX :: [L.TProp] -> LaTeX
deadnodetoLaTeX [] = fromString "x" 
deadnodetoLaTeX ((p,True):xs) = (printprop p) <> checkmark <> lnbk <> (deadnodetoLaTeX xs) 
deadnodetoLaTeX ((p,False):xs) = (printprop p) <> lnbk <> (deadnodetoLaTeX xs) 
 
 
