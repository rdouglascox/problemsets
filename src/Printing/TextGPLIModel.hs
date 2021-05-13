module Printing.TextGPLIModel (printmodel, printmodellns) where

import Data.GPLIModel
import Data.List

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
printextensions xs = "[" ++ (concat $ (intersperse "," $ map printpair xs)) ++ "]"

printpair :: (Char, [[Int]]) -> String
printpair (c,xss) = "(" ++ [c] ++ "," ++ printlists xss ++ ")"

printlists :: [[Int]] -> String
printlists [] = "[]"
printlists xss = if maximum (map length xss) == 1
                 then "[" ++ commasep (concat xss) ++ "]"
                 else "[" ++ concatMap printtuple xss ++ "]"

printtuple :: [Int] -> String
printtuple (x:[]) = show x
printtuple xs = "(" ++ commasep xs ++ ")"

commasep :: [Int] -> String
commasep n = intersperse ',' (concatMap show n)


