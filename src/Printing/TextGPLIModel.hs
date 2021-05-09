module Printing.TextGPLIModel (printmodel, printmodellns) where

import Data.GPLIModel

printmodellns :: Model -> [String]
printmodellns (Model x y z) = ["Domain: " ++ show x, "Referents: " ++ printreferents y, "Extensions: " ++ printextensions z]

printmodel :: Model -> String
printmodel (Model x y z) = "Domain: " ++ show x ++ "\n" ++ "Referents: " ++ show y ++ "\n" ++ "Extensions: " ++ printextensions z

printreferents :: [(Char,Object)] -> String
printreferents xs = "[" ++ printrefpairs xs ++ "]"

printrefpairs :: [(Char,Object)] -> String
printrefpairs (x:y:xs) = refhelper x ++ "," ++ refhelper y
printrefpairs (x:xs) = refhelper x ++ "," ++ printrefpairs xs
printrefpairs [] = []

refhelper (x,y) = "(" ++ [x] ++ "," ++ show y ++ ")" 

printextensions :: [(Char, [[Object]])] -> String
printextensions xs = "[" ++ concatMap printpair xs ++ "]"

printpair :: (Char, [[Object]]) -> String
printpair (c,xss) = "(" ++ [c] ++ "," ++ printlists xss ++ ")"

printlists :: [[Object]] -> String
printlists xss = "[" ++ concatMap printtuple xss ++ "]"

printtuple :: [Object] -> String
printtuple (x:[]) = show x
printtuple xs = "(" ++ commasep xs ++ ")"

commasep :: [Object] -> String
commasep (x:[]) = show x
commasep xs = commasep1 xs

commasep1 :: [Object] -> String
commasep1 (x:y:[]) = show x ++ "," ++ show y
commasep1 (x:xs) = show x ++ "," ++ commasep1 xs
commasep1 [] = []

