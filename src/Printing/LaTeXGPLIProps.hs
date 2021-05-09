{-# LANGUAGE OverloadedStrings #-}

-- Convert a Prop to its LaTeX representation  
-- Modify the definitions for the latex commands for connectives for
-- different outputs

module Printing.LaTeXGPLIProps (proptoLaTeX, printprops, printarg) where

import Text.LaTeX.Base
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Math
import Data.GPLIprop


-- | Print Props (print a list of propositions)

printprops :: [Prop] -> LaTeX
printprops [] = ""
printprops (x:[]) = proptoLaTeX x
printprops (x:xs) = proptoLaTeX x <> ", " <> printprops xs 

printarg :: [Prop] -> LaTeX
printarg [] = ""
printarg (x:[]) = proptoLaTeX x
printarg (x:y:[]) = proptoLaTeX x <> " " <> tf <> proptoLaTeX y
printarg (x:xs) = proptoLaTeX x <> ", " <> printarg xs





-- PROPTOLATEX FUNCTION


proptoLaTeX :: Prop -> LaTeX
proptoLaTeX (Atomic (Predicate 'I') [x,y]) = (termtoLaTeX [x]) =: (termtoLaTeX [y]) 
proptoLaTeX (Atomic (Predicate x) y) = (fromChar x) <> termtoLaTeX y
proptoLaTeX (Negation x) = nega <> proptoLaTeX x
proptoLaTeX (Conjunction x y) = lpar <> proptoLaTeX x <> conj <> proptoLaTeX y <> rpar
proptoLaTeX (Disjunction x y) = lpar <> proptoLaTeX x <> disj <> proptoLaTeX y <> rpar
proptoLaTeX (Conditional x y) = lpar <> proptoLaTeX x <> cond <> proptoLaTeX y <> rpar
proptoLaTeX (Biconditional x y) = lpar <> proptoLaTeX x <> bcon <> proptoLaTeX y <> rpar
proptoLaTeX (Universal x y) = uni <> (fromChar x) <> proptoLaTeX y
proptoLaTeX (Existential x y) = exi <> (fromChar x) <> proptoLaTeX y

termtoLaTeX :: [Term] -> LaTeX
termtoLaTeX ts = fromString $ termstoStrings ts

fromChar :: Char -> LaTeX
fromChar c = fromString [c]

termstoStrings :: [Term] -> [Char]
termstoStrings [] = []
termstoStrings (Variable t:ts) = t : termstoStrings ts
termstoStrings (Constant t:ts) = t : termstoStrings ts    

-- DEFINITIONS OF LATEX COMMANDS FOR CONNECTIVES 

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

-- | Universal
uni :: LaTeXC l => l
uni = math $ comm0 "forall"

-- | Existential
exi :: LaTeXC l => l
exi = math $ comm0 "exists"

-- | Parentheses
lpar = fromString "("
rpar = fromString $ ")"



