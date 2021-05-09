{-# LANGUAGE OverloadedStrings #-}

-- Convert a Prop to its LaTeX representation  
-- Modify the definitions for the latex commands for connectives for
-- different outputs

module Printing.LaTeXPLProps (printprop, printprops, printarg) where

import Text.LaTeX.Base
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Math
import Data.PLprop

-- DEFINITIONS OF LATEX COMMANDS FOR CONNECTIVES 

printprop = proptoLaTeX

printprops :: [Prop] -> LaTeX
printprops [] = ""
printprops (x:[]) = proptoLaTeX x
printprops (x:xs) = proptoLaTeX x <> ", " <> printprops xs 

printarg :: [Prop] -> LaTeX
printarg [] = ""
printarg (x:[]) = proptoLaTeX x
printarg (x:y:[]) = proptoLaTeX x <> " " <> tf <> proptoLaTeX y
printarg (x:xs) = proptoLaTeX x <> ", " <> printarg xs


tf :: LaTeXC l => l
tf = math $ comm0 "therefore"

-- | Negation
nega :: LaTeXC l => l
nega = math $ comm0 "lnot"

-- | Conjunction
conj :: LaTeXC l => l
conj = math $ comm0 "wedge"

-- | Disjunction
disj :: LaTeXC l => l
disj = math $ comm0 "vee"

-- | Conditional
cond :: LaTeXC l => l
cond = math $ comm0 "rightarrow"

-- | Biconditional
bcon :: LaTeXC l => l
bcon = math $ comm0 "leftrightarrow"

lpar = fromString "("
rpar = fromString $ ")"

-- define 
texconj = "$land$"
texdisj = "lor"
texcond = "arrow" 
texneg = "$lnot$"
texbcon = "leftrightarrow"

-- PROPTOLATEX FUNCTION

proptoLaTeX :: Prop -> LaTeX
proptoLaTeX (Basic x) = fromString x
proptoLaTeX (Negation x) = nega <> proptoLaTeX x
proptoLaTeX (Conjunction x y) = lpar <> proptoLaTeX x <> conj <> proptoLaTeX y <> rpar
proptoLaTeX (Disjunction x y) = lpar <> proptoLaTeX x <> disj <> proptoLaTeX y <> rpar
proptoLaTeX (Conditional x y) = lpar <> proptoLaTeX x <> cond <> proptoLaTeX y <> rpar
proptoLaTeX (Biconditional x y) = lpar <> proptoLaTeX x <> bcon <> proptoLaTeX y <> rpar



