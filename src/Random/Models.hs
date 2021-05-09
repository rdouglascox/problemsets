module Random.Models where

-- |module should export a function that makes a random model for a
-- given proposition or list of propositions.

import Data.GPLIModel
import Data.GPLIprop

import System.Random
import Data.List


-- |Takes a list of propositions and returns a random model
rmodel :: [Prop] -> Model
rmodel = undefined

-- | get a random element from a list
r :: RandomGen g => g -> [a] -> a
r g x = x!!(fst(randomR(0,((length x)) -1) g))


-- | get a random stream from a list
rs :: RandomGen g => g -> [a] -> [a]
rs g x = r g1 x : rs g2 x
    where (g1,g2) = split g


rdomain :: RandomGen g => g -> [Int]
rdomain g = r g [[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5]]

rreferents :: RandomGen g => g -> Prop -> [Int] -> [(Char,Int)] 
rreferents g p d = zip (getrefs2 p) (rs g d)

getrefs2 :: Prop -> [Char]
getrefs2 p = nub $ getrefs1 p

getrefs :: [Term] -> [Char]
getrefs [] = []
getrefs (Variable x:xs) = getrefs xs
getrefs (Constant x:xs) = x : getrefs xs

getrefs1 :: Prop -> [Char]
getrefs1 (Atomic _ x) = getrefs x
getrefs1 (Conjunction l r) = getrefs1 l ++ getrefs1 r
getrefs1 (Disjunction l r) = getrefs1 l ++ getrefs1 r
getrefs1 (Conditional l r) = getrefs1 l ++ getrefs1 r
getrefs1 (Biconditional l r) = getrefs1 l ++ getrefs1 r
getrefs1 (Negation x) = getrefs1 x
getrefs1 (Existential x s) = getrefs1 s
getrefs1 (Universal x s) = getrefs1 s


rextensions :: RandomGen g => g -> Prop -> [Int] -> [(Char,[[Int]])]  
rextensions g p d =  undefined

-- |for each n place predicate, we generate a list of lists of length
-- n taken from the domain. 


helper :: RandomGen g => g -> [(Char,Int)] -> [Int] -> [(Char,[[Int]])]
helper g [] ns = []
helper g ((c,n):xs) ns = (c, nub $ (rtake g ((length ns)^(length ns)) (chop n (rs g ns)))) : helper g xs ns

rtake :: RandomGen g => g -> Int -> [a] -> [a]
rtake g n xs = take ((r g [0..n])::Int) xs 

chop :: Int -> [Int] -> [[Int]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs) 


getpreds2 :: Prop -> [(Char,Int)]
getpreds2 p = nub $ getpreds1 p

getpreds1 :: Prop -> [(Char,Int)]
getpreds1 (Atomic (Predicate y) x) = [(y,length x)]
getpreds1 (Conjunction l r) = getpreds1 l ++ getpreds1 r
getpreds1 (Disjunction l r) = getpreds1 l ++ getpreds1 r
getpreds1 (Conditional l r) = getpreds1 l ++ getpreds1 r
getpreds1 (Biconditional l r) = getpreds1 l ++ getpreds1 r
getpreds1 (Negation x) = getpreds1 x
getpreds1 (Existential x s) = getpreds1 s
getpreds1 (Universal x s) = getpreds1 s




