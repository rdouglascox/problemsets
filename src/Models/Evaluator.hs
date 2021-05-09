module Models.Evaluator (meval) where

-- |This module provides an evaluator for propositions on models

import Data.GPLIModel
import Data.GPLIprop

import Data.List
import Data.Maybe

-- | evaluate a proposition in a model
meval :: [Prop] -> Model -> Bool
meval ps m = let prop = toconj ps in
             val m (genass m prop) prop

-- | turn a list of proposition into a conjunction
toconj :: [Prop] -> Prop
toconj (x:[]) = x
toconj xs = foldl1 Conjunction xs 

-- | an assigment is a list of char int pairs
type Assignment = [(Char,Int)]

-- | get the extension of a predicate
getextension :: Model -> Char -> [[Int]]
getextension m x = fromJust $ lookup x (extensions m)

-- | get the referents of a list of terms
getreferent :: Model -> Assignment -> [Term] -> [Int]
getreferent m g xs = map f xs
    where f (Constant x) = fromJust $ lookup x (referents m)
          f (Variable x) = fromJust $ lookup x g 

-- | get the extension of 'I' in a model
identity :: Model -> [[Int]]
identity x = [(x:x:[]) | x <- (domain x)]

-- | evaluate a proposition on a model and an assignment
val :: Model -> Assignment -> Prop -> Bool
val m g (Atomic (Predicate 'I') pair@(x:y:[])) = getreferent m g pair `elem` identity m 
val m g (Atomic (Predicate x) y) = getreferent m g y `elem` getextension m x
val m g (Negation x) = not (val m g x)
val m g (Conjunction l r) = and [val m g l, val m g r]
val m g (Disjunction l r) = or [val m g l, val m g r]
val m g (Conditional l r) = or [not (val m g l), val m g r]
val m g (Biconditional l r) = and [val m g (Conditional l r), val m g (Conditional r l)]
val m g (Existential x s) = or (map (\z -> (val m (newa g x z) s )) (domain m))
val m g (Universal x s) = and (map (\z -> (val m (newa g x z) s )) (domain m))

-- | generate a new assignment from an old assignment
newa :: Assignment -> Char -> Int -> Assignment
newa g c i = [(c,i)] ++ [x | x <- g, (fst x) /= c]

-- | generates an initial assignment of referents to variables
genass :: Model -> Prop -> Assignment
genass m ts = [(x, head (domain m)) | x <- (getvars1 ts)]

getvars :: [Term] -> [Char]
getvars [] = []
getvars (Variable x:xs) = x : getvars xs
getvars (Constant x:xs) = getvars xs

getvars1 :: Prop -> [Char]
getvars1 (Atomic _ x) = getvars x
getvars1 (Conjunction l r) = getvars1 l ++ getvars1 r
getvars1 (Disjunction l r) = getvars1 l ++ getvars1 r
getvars1 (Conditional l r) = getvars1 l ++ getvars1 r
getvars1 (Biconditional l r) = getvars1 l ++ getvars1 r
getvars1 (Negation x) = getvars1 x
getvars1 (Existential x s) = getvars1 s
getvars1 (Universal x s) = getvars1 s

