module Printing.PLprop where

import Data.PLprop
import Data.List

printPLprop :: Prop -> String
printPLprop (Basic x) = x
printPLprop (Negation x) = "~" ++ printPLprop x
printPLprop (Conjunction x y) = "(" ++ (printPLprop x) ++ "&" ++ (printPLprop y) ++ ")" 
printPLprop (Disjunction x y) = "(" ++ (printPLprop x) ++ "v" ++ (printPLprop y) ++ ")"
printPLprop (Conditional x y) = "(" ++ (printPLprop x) ++ "->" ++ (printPLprop y) ++ ")"
printPLprop (Biconditional x y) = "(" ++ (printPLprop x) ++ "<->" ++ (printPLprop y) ++ ")"

printPLprops :: [Prop] -> String
printPLprops ps = concat $ intersperse ", " (map printPLprop ps)
