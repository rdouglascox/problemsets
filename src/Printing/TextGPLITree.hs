module Printing.TextGPLITree (printProp, printPTree) where

import Data.Tree
import Data.GPLIprop
import Data.GPLITree
import Data.List


 
-- |Pretty Printing

-- |Simple Print
sp = putStrLn . printPTree

printPTree :: PTree -> String
printPTree = drawTree . ptree2Tree

ptree2Tree :: PTree -> Tree String
ptree2Tree (Branch xs (l,r)) = Node (printAProps xs) [ptree2Tree l, ptree2Tree r] 
ptree2Tree (Leaf xs) = Node ((printAProps xs) ++ " <- open") []
ptree2Tree (DeadLeaf xs) = Node ((printAProps xs) ++ " x") [] 

printAProps :: [AProp] -> String
printAProps [] = []
printAProps (x:xs) = printAProp x ++ ", " ++ printAProps xs 

printAProp :: AProp -> String
printAProp (AProp p True s) = printProp p ++ " checked" ++ " " ++ sort s 
printAProp (AProp p False s) = printProp p ++ " " ++ sort s 

printProp :: Prop -> String
printProp (Atomic (Predicate x) xs) = [x] ++ printTerms xs
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


