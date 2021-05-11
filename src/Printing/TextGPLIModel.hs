module Printing.TextGPLIModel (printmodel, printmodellns) where

import Data.GPLIModel

printmodellns :: Model -> [String]
printmodellns (Model x y z) = ["Domain: " ++ show x, "Referents: " ++ printreferents y, "Extensions: " ++ printextensions z]

printmodel :: Model -> String
printmodel (Model x y z) = "Domain: " ++ show x ++ "\n" ++ "Referents: " ++ show y ++ "\n" ++ "Extensions: " ++ printextensions z

printreferents :: [(Char,Int)] -> String
printreferents xs = "[" ++ printrefpairs xs ++ "]"

printrefpairs :: [(Char,Int)] -> String
printrefpairs (x:y:[]) = refhelper x ++ "," ++ refhelper y
printrefpairs (x:[]) = refhelper x
printrefpairs (x:xs) = refhelper x ++ "," ++ printrefpairs xs
printrefpairs [] = []

refhelper (x,y) = "(" ++ [x] ++ "," ++ show y ++ ")" 

printextensions :: [(Char, [[Int]])] -> String
printextensions xs = "[" ++ concatMap printpair xs ++ "]"

printpair :: (Char, [[Int]]) -> String
printpair (c,xss) = "(" ++ [c] ++ "," ++ printlists xss ++ ")"

printlists :: [[Int]] -> String
printlists (x:[]) = "[" ++ commasep x ++ "]"
printlists xss = "[" ++ concatMap printtuple xss ++ "]"

printtuple :: [Int] -> String
printtuple (x:[]) = show x
printtuple xs = "(" ++ commasep xs ++ ")"

commasep :: [Int] -> String
commasep (x:[]) = show x
commasep xs = commasep1 xs

commasep1 :: [Int] -> String
commasep1 (x:y:[]) = show x ++ "," ++ show y
commasep1 (x:[]) = show x
commasep1 (x:xs) = show x ++ "," ++ commasep1 xs
commasep1 [] = []

