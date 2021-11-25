module Printing.UnicodeGPLIProps (printprop, printprops, printarg) where

import Printing.UnicodeLogicSymbols
import Data.GPLIprop

printprop :: Prop -> String
printprop p = case p of
    Atomic (Predicate p) x -> p: printterms x
    Negation p -> uNeg ++ printprop p
    Conjunction l r -> "(" ++ printprop l ++ uAnd ++ printprop r ++ ")"
    Disjunction l r -> "(" ++ printprop l ++ uOr ++ printprop r ++ ")"
    Conditional l r -> "(" ++ printprop l ++ uIf ++ printprop r ++ ")"
    Biconditional l r -> "(" ++ printprop l ++ uIff ++ printprop r ++ ")"
    Universal c p -> uUni ++ (c: printprop p)
    Existential c p -> uExi ++ (c: printprop p)

printterms :: [Term] -> String 
printterms [] = ""
printterms (x:xs) = case x of
    Variable x -> x : printterms xs
    Constant x -> x : printterms xs

printprops :: [Prop] -> String
printprops [] = ""
printprops [x] = printprop x
printprops (x:xs) = printprop x <> ", " <> printprops xs

printarg :: [Prop] -> String
printarg [] = ""
printarg [x] = printprop x
printarg [x, y] = printprop x <> " " <> tf <> printprop y
printarg (x:xs) = printprop x <> ", " <> printarg xs

tf :: String
tf = "∴"