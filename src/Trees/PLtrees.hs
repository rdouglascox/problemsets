{-|
Module      : Trees.PLtrees
Description : Make tree proofs for PL
Copyright   : (c) Ryan Cox 2021
License     : BSD
Maintainer  : ryan@rdouglascox.com
Stability   : experimental

The module exports the mktree function for generating tree proofs from
PL.
-}


module Trees.PLtrees (mktree,minBranch,maxBranch,maxPath,minPath,allpathsclosed,maxpath,nbranches,TProp(..),Tree(..)) where

import Data.List
import Data.PLprop
import System.Random
import Printing.PLprop


-- |Binary branching trees
data Tree a = Leaf a | DeadLeaf a | Branch a (Tree a, Tree a)
    deriving (Eq,Show)

-- |Trees are built from "tagged" propositions
type TProp = (Prop,Bool)

addb :: ([TProp],[TProp]) -> Tree [TProp] -> Tree [TProp]
addb ([],[]) x = x
addb (l,r) (Branch xs (x,y)) = Branch xs (addb (l,r) x, addb (l,r) y)
addb (l,r) (Leaf xs) = Branch xs (Leaf l, Leaf r)
addb (l,r) (DeadLeaf xs) = DeadLeaf xs

addnb :: [TProp] -> Tree [TProp] -> Tree [TProp]
addnb [] x = x
addnb l (Branch xs (x,y)) = Branch xs (addnb l x, addnb l y)
addnb l (Leaf xs) = Leaf (xs ++ l)
addnb l (DeadLeaf xs) = DeadLeaf xs

--- conditional

condtag :: [TProp] -> [TProp]
condtag [] = []
condtag ((Conditional x y,False):xs) = (Conditional x y,True) : condtag xs 
condtag (x:xs) = x : condtag xs

condget :: [TProp] -> ([TProp],[TProp])
condget [] = ([],[])
condget ((Conditional x y,False):xs) = ([(Negation x,False)], [(y,False)]) 
condget (x:xs) = condget xs    

condrule :: Tree [TProp] -> Tree [TProp]
condrule (Branch xs (y,z)) = if condget xs == ([],[]) 
                             then (Branch xs (condrule y, condrule z))
                             else addb (condget xs) (Branch (condtag xs) (y,z))
condrule (Leaf xs) = addb (condget xs) (Leaf (condtag xs)) 
condrule (DeadLeaf xs) = DeadLeaf xs


--- disjunction

disjtag :: [TProp] -> [TProp]
disjtag [] = []
disjtag ((Disjunction x y,False):xs) = (Disjunction x y,True) : disjtag xs 
disjtag (x:xs) = x : disjtag xs

disjget :: [TProp] -> ([TProp],[TProp])
disjget [] = ([],[])
disjget ((Disjunction x y,False):xs) = ([(x,False)], [(y,False)]) 
disjget (x:xs) = disjget xs    

disjrule :: Tree [TProp] -> Tree [TProp]
disjrule (Branch xs (y,z)) = if disjget xs == ([],[])
                             then (Branch xs (disjrule y, disjrule z))
                             else addb (disjget xs) (Branch (disjtag xs) (y,z))
disjrule (Leaf xs) = addb (disjget xs) (Leaf (disjtag xs)) 
disjrule (DeadLeaf xs) = DeadLeaf xs

--- negated conjunction

negconjtag :: [TProp] -> [TProp]
negconjtag [] = []
negconjtag ((Negation (Conjunction x y),False):xs) = (Negation (Conjunction x y),True) : negconjtag xs 
negconjtag (x:xs) = x : negconjtag xs

negconjget :: [TProp] -> ([TProp],[TProp])
negconjget [] = ([],[])
negconjget ((Negation (Conjunction x y),False):xs) = ([(Negation x,False)], [(Negation y,False)]) 
negconjget (x:xs) = negconjget xs    

negconjrule :: Tree [TProp] -> Tree [TProp]
negconjrule (Branch xs (y,z)) = if negconjget xs == ([],[])
                                then (Branch xs (negconjrule y, negconjrule z))
                                else addb (negconjget xs) (Branch (negconjtag xs) (y,z))
negconjrule (Leaf xs) = addb (negconjget xs) (Leaf (negconjtag xs)) 
negconjrule (DeadLeaf xs) = DeadLeaf xs

--- biconditional

bicondtag :: [TProp] -> [TProp]
bicondtag [] = []
bicondtag ((Biconditional x y,False):xs) = (Biconditional x y,True) : bicondtag xs 
bicondtag (x:xs) = x : bicondtag xs

bicondget :: [TProp] -> ([TProp],[TProp])
bicondget [] = ([],[])
bicondget ((Biconditional x y,False):xs) = ([(x,False),(y,False)], [(Negation x,False),(Negation y,False)]) 
bicondget (x:xs) = bicondget xs    

bicondrule :: Tree [TProp] -> Tree [TProp]
bicondrule (Branch xs (y,z)) = if bicondget xs == ([],[])
                               then (Branch xs (bicondrule y, bicondrule z))
                               else addb (bicondget xs) (Branch (bicondtag xs) (y,z))
bicondrule (Leaf xs) = addb (bicondget xs) (Leaf (bicondtag xs)) 
bicondrule (DeadLeaf xs) = DeadLeaf xs

--- negated biconditional

negbicontag :: [TProp] -> [TProp]
negbicontag [] = []
negbicontag ((Negation (Biconditional x y),False):xs) = (Negation (Biconditional x y),True) : negbicontag xs 
negbicontag (x:xs) = x : negbicontag xs

negbiconget :: [TProp] -> ([TProp],[TProp])
negbiconget [] = ([],[])
negbiconget ((Negation (Biconditional x y),False):xs) = ([(Negation x,False),(y,False)], [(x,False),(Negation y,False)]) 
negbiconget (x:xs) = negbiconget xs    

negbiconrule :: Tree [TProp] -> Tree [TProp]
negbiconrule (Branch xs (y,z)) = if negbiconget xs == ([],[])
                                 then Branch xs (negbiconrule y, negbiconrule z)
                                 else addb (negbiconget xs) (Branch (negbicontag xs) (y,z))
negbiconrule (Leaf xs) = addb (negbiconget xs) (Leaf (negbicontag xs)) 
negbiconrule (DeadLeaf xs) = DeadLeaf xs

--- conjunction

conjtag :: [TProp] -> [TProp]
conjtag [] = []
conjtag ((Conjunction x y,False):xs) = (Conjunction x y,True) : conjtag xs 
conjtag (x:xs) = x : conjtag xs

conjget :: [TProp] -> [TProp]
conjget [] = []
conjget ((Conjunction x y,False):xs) = [(x,False),(y,False)] 
conjget (x:xs) = conjget xs    

conjrule :: Tree [TProp] -> Tree [TProp]
conjrule (Branch xs (y,z)) = if conjget xs == []
                             then Branch xs (conjrule y,conjrule z)
                             else addnb (conjget xs) (Branch (conjtag xs) (y,z))
conjrule (Leaf xs) = addnb (conjget xs) (Leaf (conjtag xs)) 
conjrule (DeadLeaf xs) = DeadLeaf xs

--- negated disjunction

negdisjtag :: [TProp] -> [TProp]
negdisjtag [] = []
negdisjtag ((Negation (Disjunction x y),False):xs) = (Negation (Disjunction x y),True) : negdisjtag xs 
negdisjtag (x:xs) = x : negdisjtag xs

negdisjget :: [TProp] -> [TProp]
negdisjget [] = []
negdisjget ((Negation (Disjunction x y),False):xs) = [(Negation x,False),(Negation y,False)] 
negdisjget (x:xs) = negdisjget xs    

negdisjrule :: Tree [TProp] -> Tree [TProp]
negdisjrule (Branch xs (y,z)) = if negdisjget xs == []
                                then Branch xs (negdisjrule y, negdisjrule z)
                                else addnb (negdisjget xs) (Branch (negdisjtag xs) (y,z))
negdisjrule (Leaf xs) = addnb (negdisjget xs) (Leaf (negdisjtag xs)) 
negdisjrule (DeadLeaf xs) = DeadLeaf xs

--- negated conditional

negcondtag :: [TProp] -> [TProp]
negcondtag [] = []
negcondtag ((Negation (Conditional x y),False):xs) = (Negation (Conditional x y),True) : negcondtag xs 
negcondtag (x:xs) = x : negcondtag xs

negcondget :: [TProp] -> [TProp]
negcondget [] = []
negcondget ((Negation (Conditional x y),False):xs) = [(x,False),(Negation y,False)] 
negcondget (x:xs) = negcondget xs    

negcondrule :: Tree [TProp] -> Tree [TProp]
negcondrule (Branch xs (y,z)) = if negcondget xs == []
                                then Branch xs (negcondrule y, negcondrule z)
                                else addnb (negcondget xs) (Branch (negcondtag xs) (y,z))
negcondrule (Leaf xs) = addnb (negcondget xs) (Leaf (negcondtag xs)) 
negcondrule (DeadLeaf xs) = DeadLeaf xs

--- double negation

doublenegtag :: [TProp] -> [TProp]
doublenegtag [] = []
doublenegtag ((Negation (Negation x),False):xs) = (Negation (Negation x),True) : doublenegtag xs 
doublenegtag (x:xs) = x : doublenegtag xs

doublenegget :: [TProp] -> [TProp]
doublenegget [] = []
doublenegget ((Negation (Negation x),False):xs) = [(x,False)] 
doublenegget (x:xs) = doublenegget xs    

doublenegrule :: Tree [TProp] -> Tree [TProp]
doublenegrule (Branch xs (y,z)) = if doublenegget xs == []
                                  then Branch xs (doublenegrule y, doublenegrule z)
                                  else addnb (doublenegget xs) (Branch (doublenegtag xs) (y,z))
doublenegrule (Leaf xs) = addnb (doublenegget xs) (Leaf (doublenegtag xs)) 
doublenegrule (DeadLeaf xs) = DeadLeaf xs

--- close paths

cfc :: [TProp] -> Tree [TProp] -> Tree [TProp]
cfc acc (Branch xs (y,z)) = Branch xs (cfc (acc ++ xs) y, cfc (acc ++ xs) z) 
cfc acc (Leaf xs) = if contra (acc ++ xs) then DeadLeaf xs else Leaf xs 
cfc acc (DeadLeaf xs) = DeadLeaf xs 

contra :: [TProp] -> Bool
contra xs = or (map (\x -> ((x `elem` (map fst xs)) && (Negation x) `elem` (map fst xs))) (map fst xs))

-- apply rule, check for contradictions

ap :: (Tree [TProp] -> Tree [TProp]) -> Tree [TProp] -> Tree [TProp]
ap r t = (cfc [] . r) t 

-- loop a rule until it makes no difference

lp :: (Tree [TProp] -> Tree [TProp]) -> Tree [TProp] -> Tree [TProp]
lp r t = if r t == r (r t) then r t else lp r (r t)

-- combine rules

allnb :: Tree [TProp] -> Tree [TProp]
allnb = (lp (ap conjrule)) . (lp (ap negdisjrule)) . (lp (ap negcondrule)) . (lp (ap doublenegrule))  

allb :: Tree [TProp] -> Tree [TProp]
allb = (lp allnb) . (lp (ap negbiconrule)) . (lp allnb) . (lp (ap bicondrule)) . (lp allnb) . (lp (ap condrule)) . (lp allnb) . (lp (ap negconjrule)) . (lp allnb) . (lp (ap disjrule)) . (lp allnb)   

-- prep tree

preptree :: [Prop] -> Tree [TProp]
preptree xs = Leaf (map (\x -> (x,False)) xs)   

-- |Returns a tree proof from a given list of propositions.
mktree :: [Prop] -> Tree [TProp]
mktree t = lp allb (preptree t)

-- text printer

prettyprops :: [TProp] -> String
prettyprops [] = []
prettyprops (x:[]) = if snd x == True
    then (printPLprop (fst x)) ++ " checked" 
    else (printPLprop (fst x)) ++ ""
prettyprops (x:xs) = if snd x == True
    then (printPLprop (fst x)) ++ " checked, " ++ prettyprops xs   
    else (printPLprop (fst x)) ++ ", " ++ prettyprops xs

asciitree xs = mapM_ putStrLn $ linenumbers (prettyprintHelper xs)

prettyprint (Branch x (left,right)) = unlines (prettyprintHelper (Branch x (left,right))) 

linenumbers xs = [ (show x) ++ "." ++ spaces (length (show x)) ++ y | (x,y) <- (zip [1..] xs)]

spaces :: Int -> String
spaces x = concat [ " " | y <- [0..(3 - x)]]

prettyprintHelper :: Tree [TProp] -> [String]
prettyprintHelper (Branch x (left,right)) = prettyprops x : (prettyPrintSubtree left right)
    where 
        prettyPrintSubtree left right = ((pad "├─ " "│  ") (prettyprintHelper right)) ++ ((pad "└─ " "   ") (prettyprintHelper left))
        pad first rest = zipWith (++) (first : repeat rest)
prettyprintHelper (Leaf x) = [prettyprops x ++ ", <- open path"]
prettyprintHelper (DeadLeaf x) =  [prettyprops x ++ " x"]

-- tree stats

nbranches :: Tree [TProp] -> Int
nbranches (Branch xs (z,y)) = 1 + (nbranches z) + (nbranches y) 
nbranches (Leaf xs) = 0  
nbranches (DeadLeaf xs) = 0 

maxpath :: Tree [TProp] -> Int
maxpath (Branch xs (z,y)) = (length xs) + (maximum [(maxpath z),(maxpath y)])
maxpath (Leaf xs) = length xs
maxpath (DeadLeaf xs) = length xs

allpathsclosed :: Tree [TProp] -> Bool
allpathsclosed (Branch _ (l,r)) = allpathsclosed l && allpathsclosed r
allpathsclosed (Leaf _) = False
allpathsclosed (DeadLeaf _) = True

-- |max path
maxPath :: Int -> Tree [TProp] -> Bool
maxPath n t = (maximum $ lengthPaths t) <= n

-- |min path
minPath :: Int -> Tree [TProp]-> Bool
minPath n t = (minimum $ lengthPaths t) >= n

-- |path lengths
lengthPaths :: Tree [TProp]-> [Int]
lengthPaths = lengthPaths1 []

lengthPaths1 :: [Int] -> Tree [TProp]-> [Int]
lengthPaths1 n (Branch xs (l,r)) = lengthPaths1 [length xs] l ++ lengthPaths1 [length xs] r
lengthPaths1 n (Leaf xs) = length xs : n
lengthPaths1 n (DeadLeaf xs) = length xs : n


-- |max branch
maxBranch :: Int -> Tree [TProp]-> Bool
maxBranch n t = numBranch t <= n

-- |min branch
minBranch :: Int -> Tree [TProp]-> Bool
minBranch n t = numBranch t >= n

-- | numBranch
numBranch :: Tree [TProp]-> Int
numBranch (Branch xs (l,r)) = 1 + numBranch l + numBranch r
numBranch _ = 0


