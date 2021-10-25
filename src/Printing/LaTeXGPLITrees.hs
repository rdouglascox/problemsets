{-# LANGUAGE OverloadedStrings #-}


module Printing.LaTeXGPLITrees (printtree, gplitreetoLaTeX) where

import Text.LaTeX
import Text.LaTeX.Packages.Geometry
import Text.LaTeX.Packages.Inputenc
import Text.LaTeX.Base
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Math
import Text.LaTeX.Base.Pretty
import Text.LaTeX.Packages.Trees.Qtree
import Text.LaTeX.Packages.AMSSymb
import Printing.LaTeXGPLIProps
import Data.GPLIprop
import qualified Data.GPLITree as L

printtree :: L.PTree -> LaTeX 
printtree t = (tree id $ (gplitreetoLaTeX t))


-- ordinary trees

gplitreetoLaTeX :: L.PTree -> Tree LaTeX
gplitreetoLaTeX (L.Leaf xs) = Node Nothing [Leaf $ nodetoLaTeX $ xs]
gplitreetoLaTeX (L.DeadLeaf xs) = Node Nothing [Leaf $ deadnodetoLaTeX $ xs]
gplitreetoLaTeX (L.Branch xs (l,r)) = Node (Just $ nodetoLaTeX' $ xs) [gplitreetoLaTeX' l, gplitreetoLaTeX' r]

gplitreetoLaTeX' :: L.PTree -> Tree LaTeX
gplitreetoLaTeX' (L.Leaf xs) = Leaf $ nodetoLaTeX $ xs
gplitreetoLaTeX' (L.DeadLeaf xs) = Leaf $ deadnodetoLaTeX $ xs
gplitreetoLaTeX' (L.Branch xs (l,r)) = Node (Just $ nodetoLaTeX' $ xs) [gplitreetoLaTeX' l, gplitreetoLaTeX' r]

nodetoLaTeX' :: [L.AProp] -> LaTeX
nodetoLaTeX' [] = fromString "" 
nodetoLaTeX' ((L.AProp p True subs):xs) = (proptoLaTeX p) <> checkmark <> (fromString $ reverse subs) <> lnbk <> (nodetoLaTeX' xs) 
nodetoLaTeX' ((L.AProp (Universal x p) False subs):xs) = case subs of (y:ys) -> (proptoLaTeX (Universal x p)) <> (fromString "\\") <> (fromString $ reverse subs) <> lnbk <> (nodetoLaTeX' xs) 
                                                                      [] -> (proptoLaTeX (Universal x p)) <> (fromString $ reverse subs) <> lnbk <> (nodetoLaTeX' xs) 
 
nodetoLaTeX' ((L.AProp p False subs):xs) = (proptoLaTeX p) <> (fromString $ reverse subs) <> lnbk <> (nodetoLaTeX' xs) 

nodetoLaTeX :: [L.AProp] -> LaTeX
nodetoLaTeX [] = fromString "o" 
nodetoLaTeX ((L.AProp p True subs):xs) = (proptoLaTeX p) <> checkmark <> (fromString $ reverse subs) <> lnbk <> (nodetoLaTeX xs) 
nodetoLaTeX ((L.AProp (Universal x p) False subs):xs) = case subs of (y:ys) -> (proptoLaTeX (Universal x p)) <> (fromString "\\") <> (fromString $ reverse subs) <> lnbk <> (nodetoLaTeX xs) 
                                                                     [] -> (proptoLaTeX (Universal x p)) <> (fromString $ reverse subs) <> lnbk <> (nodetoLaTeX xs) 
 
nodetoLaTeX ((L.AProp p False subs):xs) = (proptoLaTeX p) <> (fromString $ reverse subs) <> lnbk <> (nodetoLaTeX xs) 

deadnodetoLaTeX :: [L.AProp] -> LaTeX
deadnodetoLaTeX [] = fromString "x" 
deadnodetoLaTeX ((L.AProp p True subs):xs) = (proptoLaTeX p) <> checkmark <> (fromString $ reverse subs) <> lnbk <> (deadnodetoLaTeX xs) 
deadnodetoLaTeX ((L.AProp (Universal x p) False subs):xs) = case subs of (y:ys) -> (proptoLaTeX (Universal x p)) <> (fromString "\\") <> (fromString $ reverse subs) <> lnbk <> (deadnodetoLaTeX xs) 
                                                                         [] -> (proptoLaTeX (Universal x p)) <> (fromString $ reverse subs) <> lnbk <> (deadnodetoLaTeX xs) 

deadnodetoLaTeX ((L.AProp p False subs):xs) = (proptoLaTeX p) <> (fromString $ reverse subs) <> lnbk <> (deadnodetoLaTeX xs) 
 
