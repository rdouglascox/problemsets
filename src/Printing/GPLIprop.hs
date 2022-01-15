module Printing.GPLIprop (printGPLIprops) where

import Data.GPLIprop

import Data.List


printProp :: Prop -> String
printProp (Atomic (Predicate x) xs) = x : printTerms xs
printProp (Negation l) = "~" ++ printProp l
printProp (Conjunction l r) = "(" ++ printProp l ++ "&" ++ printProp r ++ ")"
printProp (Disjunction l r) = "(" ++ printProp l ++ "v" ++ printProp r ++ ")"
printProp (Conditional l r) = "(" ++ printProp l ++ "->" ++ printProp r ++ ")"
printProp (Biconditional l r) = "(" ++ printProp l ++ "<->" ++ printProp r ++ ")"
printProp (Universal x p) = "@" ++ [x] ++ printProp p
printProp (Existential x p) = "#" ++ [x] ++ printProp p

printTerms :: [Term] -> String
printTerms [] = []
printTerms (Variable x:xs) = x : printTerms xs
printTerms (Constant x:xs) = x : printTerms xs

printGPLIprops :: [Prop] -> String
printGPLIprops ps = intercalate ", " (map printProp ps)