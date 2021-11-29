{-# LANGUAGE TupleSections #-}

module Trees.PLtreesNew (mktreeSafe,mktree) where

import Data.PLprop ( Prop(..) )
import Data.PLTree

mktreeSafe :: [Prop] -> Maybe (Tree [TProp])
mktreeSafe = Just . mktreeSimple

mktree :: [Prop] -> Tree [TProp]
mktree =  mktreeSimple

applyBranching :: ([TProp],[TProp]) -> Tree [TProp] -> Tree [TProp]
applyBranching (l,r) (Branch ps (lb,rb)) = Branch ps (applyBranching (l,r) lb,applyBranching (l,r) rb)
applyBranching (l,r) (Leaf ps) = Branch ps (Leaf l,Leaf r)
applyBranching (l,r) (DeadLeaf ps) = DeadLeaf ps

applyNonBranching :: [TProp] -> Tree [TProp] -> Tree [TProp]
applyNonBranching x (Branch ps (lb,rb)) = Branch ps (applyNonBranching x lb,applyNonBranching x rb)
applyNonBranching x (Leaf ps) = Leaf (ps ++ x)
applyNonBranching x (DeadLeaf ps) = DeadLeaf ps

doubleNegationRuleDef :: TProp -> [TProp]
doubleNegationRuleDef (Negation (Negation p),_) = [(p, False)]

conjunctionRuleDef :: TProp -> [TProp]
conjunctionRuleDef (Conjunction l r, _) = [(l, False),(r,False)]

negdisjunctionRuleDef :: TProp -> [TProp]
negdisjunctionRuleDef (Negation (Disjunction l r), _) = [(Negation l, False), (Negation r,False)]

negconditionalRuleDef :: TProp -> [TProp]
negconditionalRuleDef (Negation (Conditional l r), _) = [(l, False), (Negation r, False)]

disjunctionRuleDef :: TProp -> ([TProp],[TProp])
disjunctionRuleDef (Disjunction l r, _) = ([(l,False)],[(r,False)])

conditionalRuleDef :: TProp -> ([TProp],[TProp])
conditionalRuleDef (Conditional l r,_) = ([(Negation l, False)], [(r,False)])

negconjunctionRuleDef :: TProp -> ([TProp],[TProp])
negconjunctionRuleDef (Negation (Conjunction l r),_) = ([(Negation r, False)], [(Negation l, False)])

biconditionalRuleDef :: TProp -> ([TProp],[TProp])
biconditionalRuleDef (Biconditional l r,_) = ([(l,False), (r, False)],[(Negation l, False), (Negation r, False)])
    
negbiconditionalRuleDef :: TProp -> ([TProp],[TProp])
negbiconditionalRuleDef (Negation (Biconditional l r),_) = ([(Negation l, False), (r, False)],[(l,False), (Negation r, False)])

select :: TProp -> Tree [TProp] -> Tree [TProp]
select p@(Negation (Negation l), _) = applyNonBranching (doubleNegationRuleDef p)
select p@(Conjunction l r, _) = applyNonBranching (conjunctionRuleDef p)
select p@(Negation (Disjunction l r), _) = applyNonBranching (negdisjunctionRuleDef p)
select p@(Negation (Conditional l r), _) = applyNonBranching (negconditionalRuleDef p)
select p@(Disjunction l r, _) = applyBranching (disjunctionRuleDef p)
select p@(Conditional l r,_) = applyBranching (conditionalRuleDef p)
select p@(Negation (Conjunction l r),_) = applyBranching (negconjunctionRuleDef p)
select p@(Biconditional l r,_) = applyBranching (biconditionalRuleDef p)
select p@(Negation (Biconditional l r),_) = applyBranching (negbiconditionalRuleDef p)
select p@(Negation x,  _) = id
select p@(Basic x,  _) = id

applyRule :: Tree [TProp] -> Tree [TProp]
applyRule = applyRule1 []

applyRule1 :: [TProp] -> Tree [TProp] -> Tree [TProp]
applyRule1 acc (Branch ps x) = applyRuleB acc (Branch ps x)
applyRule1 acc (Leaf ps) = applyRuleL acc (Leaf ps)
applyRule1 acc (DeadLeaf ps) = DeadLeaf ps

applyRuleB :: [TProp] -> Tree [TProp] -> Tree [TProp]
applyRuleB acc (Branch (p@(Basic q, False):ps) (lb,rb)) = applyRuleB (acc ++ [p]) (Branch ps (lb,rb))
applyRuleB acc (Branch (p@(Negation (Basic q), False):ps) (lb,rb)) = applyRuleB (acc ++ [p]) (Branch ps (lb,rb))
applyRuleB acc (Branch ((p, False):ps) (lb,rb)) = select (p, False) (Branch (acc ++ ((p, True):ps)) (lb,rb))
applyRuleB acc (Branch (p:ps) (lb,rb)) = applyRuleB (acc ++ [p]) (Branch ps (lb,rb))
applyRuleB acc (Branch [] (lb,rb)) = Branch acc (applyRule1 [] lb,applyRule rb)

applyRuleL :: [TProp] -> Tree [TProp] -> Tree [TProp]
applyRuleL acc (Leaf (p@(Basic q, False):ps)) = applyRuleL (acc ++ [p]) (Leaf ps)
applyRuleL acc (Leaf (p@(Negation (Basic q), False):ps)) = applyRuleL (acc ++ [p]) (Leaf ps)
applyRuleL acc (Leaf ((p, False): ps)) = select (p,False) (Leaf (acc ++ (p,True): ps))
applyRuleL acc (Leaf (p:ps)) = applyRuleL (acc ++ [p]) (Leaf ps)
applyRuleL acc (Leaf []) = Leaf acc

applyClosure :: Tree [TProp] -> Tree [TProp]
applyClosure = applyClosure1 []

applyClosure1 :: [Prop] -> Tree [TProp] -> Tree [TProp]
applyClosure1 acc (Branch ps (lb,rb)) = Branch ps (applyClosure1 (pfromtp ps ++ acc) lb,applyClosure1 (pfromtp ps ++ acc) rb)
applyClosure1 acc (Leaf ps) = if check (acc ++ pfromtp ps)
    then DeadLeaf ps
    else Leaf ps
applyClosure1 acc (DeadLeaf xs) = DeadLeaf xs

pfromtp :: [TProp] -> [Prop]
pfromtp [] = []
pfromtp ((p, _):xs) = p : pfromtp xs

check :: [Prop] -> Bool
check xs = any (\x -> (x `elem` xs) && Negation x `elem` xs) xs

prepTree  :: [Prop] -> Tree [TProp]
prepTree ps = Leaf (prepTProps ps)

prepTProps :: [Prop] -> [TProp]
prepTProps = map (,False)

mktreeSimple :: [Prop] -> Tree [TProp]
mktreeSimple ps = mktreeSimple1 (prepTree ps)

mktreeSimple1 :: Tree [TProp] -> Tree [TProp]
mktreeSimple1 t = let new = applyClosure t in
    mktreeSimple2 new new

mktreeSimple2 :: Tree [TProp] -> Tree [TProp] -> Tree [TProp]
mktreeSimple2 old t = let new = (applyClosure . applyRule) t in
    if old == new
        then old
        else mktreeSimple2 new new

hasopen :: Tree [TProp] -> Bool
hasopen (Branch ps (lr,rb)) = hasopen lr || hasopen rb
hasopen (Leaf ps) = True
hasopen (DeadLeaf ps) = False

satcheckSimple :: [Prop] -> Bool
satcheckSimple t = hasopen (mktreeSimple t)