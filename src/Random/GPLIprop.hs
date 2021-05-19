module Random.GPLIprop (rprops1, printProp, prepforequiv, prepforvalidity, prepfortaut, mplsatg, mplequivg, gpltautg, gplsatg, gplisatg, gplivalg) where

import Data.GPLIprop
import System.Random
import System.IO
import Trees.GPLItrees
import Data.Maybe
import Data.List

import Settings.GPLISettings

-- |Q and A 

-- |NEW FUNCTIONS

mplequivg :: RandomGen g => g -> Settings -> [Prop]
mplequivg gen s = head $ take 1 $ requivs gen s 

mplsatg :: RandomGen g => g -> Settings -> [Prop]
mplsatg gen s = head $ take 1 $ rsats gen s

gpltautg :: RandomGen g => g -> Settings -> [Prop]
gpltautg gen s = head $ take 1 $ rtauts gen s 

gplsatg :: RandomGen g => g -> Settings -> [Prop]
gplsatg gen s = head $ take 1 $ rsats gen s 

gplisatg :: RandomGen g => g -> Settings -> [Prop]
gplisatg gen s = head $ take 1 $ rsats gen s

gplitestg :: RandomGen g => g -> Settings -> [Prop]
gplitestg gen s = head $ take 1 $ nrprops gen s 

gplivalg :: RandomGen g => g -> Settings -> [Prop]
gplivalg gen s = head $ take 1 $ rvalids gen s 


-- |GENERATE RANDOM PROPOSITIONS

-- |Generate infinite list of tautologies
rtauts :: RandomGen g => g -> Settings ->  [[Prop]]
rtauts gen s = map untaut $ filter (allpathsclosed . mktree) $ filter (requirerules (includeRules s)) $ filter (exquirerules (excludeRules s)) $ filter (superfilter s) $ filter (treeisoversized (overSize s)) $ map prepfortaut $ nrprops gen s 

rtautsStats1 :: RandomGen g => g -> Settings -> Int 
rtautsStats1 gen s =  length (filter (allpathsclosed . mktree) $ filter (requirerules (includeRules s)) $ filter (exquirerules (excludeRules s)) $ filter (superfilter s) $ filter (treeisoversized (overSize s)) $ map prepfortaut $ take 1000 $ nrprops1 gen s) `div` 10 

untaut :: [Prop] -> [Prop]
untaut [(Negation x)] = [x]

prepfortaut :: [Prop] -> [Prop]
prepfortaut ps = map (\x -> Negation x) ps

-- |Generate infinite list of contradictory pairs
rcontradictory :: RandomGen g => g -> Settings -> [[Prop]]
rcontradictory gen s = filter (not . allpathsclosed . mktree . bi) $ filter (allpathsclosed . mktree) $ nrprops1 gen s 
    where bi (x:y:[]) = [Negation (Conjunction x y)] 

-- |Generate infinite list of contrary pairs
rcontrary :: RandomGen g => g -> Settings -> [[Prop]]
rcontrary gen s = filter (allpathsclosed . mktree . bi) $ filter (allpathsclosed . mktree) $ nrprops1 gen s 
    where bi (x:y:[]) = [Negation (Conjunction x y)] 

-- |Generate infinite list of equivalent pairs
requivs :: RandomGen g => g -> Settings ->  [[Prop]]
requivs gen s = map unequiv $ filter (allpathsclosed . mktree) $ filter (requirerules (includeRules s)) $ filter (exquirerules (excludeRules s)) $ filter (superfilter s) $ filter (treeisoversized (overSize s)) $ map prepforequiv $ nrprops gen s 

unequiv :: [Prop] -> [Prop]
unequiv [(Negation (Biconditional l r))] = [l,r]

prepforequiv :: [Prop] -> [Prop]
prepforequiv [l,r] = [(Negation (Biconditional l r))]


-- |Generate infinite list of lists of valid arguments
rvalids :: RandomGen g => g -> Settings -> [[Prop]]
rvalids gen s = map unvalid $ filter (allpathsclosed . mktree) $ filter (requirerules (includeRules s)) $ filter (exquirerules (excludeRules s)) $ filter (superfilter s) $ filter (treeisoversized (overSize s)) $ map prepforvalidity $ nrprops gen s 

prepforvalidity :: [Prop] -> [Prop]
prepforvalidity xs = reverse (tail $ reverse xs) ++ [(Negation (head $ reverse xs))] 

unvalid :: [Prop] -> [Prop]
unvalid (x:y:(Negation p):[]) = x :y : p : []

-- |Generate infinite list of lists of non-sat / joint non sat props
rnonsats :: RandomGen g => g -> Settings -> [[Prop]]
rnonsats gen s = filter (allpathsclosed . mktree) $ nrprops1 gen s 

-- |Generate infinite list of lists of sat / joint sat props 
rsats :: RandomGen g => g -> Settings -> [[Prop]] 
rsats gen s = filter (not . allpathsclosed . mktree) $ nrprops1 gen s 

-- |Generate infinite list of lists of random props (no oversized, -- other stats satisfied
nrprops1 :: RandomGen g => g -> Settings -> [[Prop]]
nrprops1 gen s = filter (requirerules (includeRules s)) $ filter (exquirerules (excludeRules s)) $ filter (superfilter s) $ filter (treeisoversized (overSize s)) $ nrprops gen s 

superfilter :: Settings -> [Prop] -> Bool
superfilter s ps = (multiuni (multiUni s) tree) && (minBranch (minBranchSet s) tree) && (maxBranch (maxBranchSet s) tree) && (minPath (minPathSet s) tree) && (maxPath (maxPathSet s) tree)
    where tree = mktree ps

-- |Generate infinte list of lists of random props (basic case)
nrprops :: RandomGen g => g -> Settings -> [[Prop]]  
nrprops g s = chop (numProps s) (rprops g s)
    where chop m xs = take m xs : chop m (drop m xs)

-- |Generate infinite list of random props (no oversized trees) 
rprops1 :: RandomGen g => g -> Settings -> [Prop]
rprops1 g s = filter (treeisoversized1 (overSize s)) $ rprops g s 

-- |Generate infinite list of random props (no vacs, no open)
rprops :: RandomGen g => g -> Settings -> [Prop]
rprops g s = filter novac $ filter isclosed $ rprops' g s

-- |Generate infinite list of random props (basic case)
rprops' :: RandomGen g => g -> Settings -> [Prop]
rprops' g s = construct g1 s (take (r g4 [(minConst s)..(maxConst s)]) $ rconstructors g2 s) : rprops' g3 s
    where (g1,g2) = split g
          (g3,g4) = split g2

-- |HELPER FUNCTIONS 

-- |Get a random element from a list
r :: RandomGen g => g -> [a] -> a
r g x = x!!(fst(randomR(0,((length x)) -1) g))

-- |Split a list at a random index 
rsplitAt :: RandomGen g => g -> [a] -> ([a],[a])
rsplitAt g xs = let y = (fst $ randomR (0,(length xs)) g) in
                splitAt y xs 

-- |Generate a list of random generators
gens :: RandomGen g => g -> [g]
gens g = g : gens (snd $ next g)

-- |FUNCTIONS TO CONSTRUCT A PROPOSITION FROM A LIST OF RANDOM CONSTRUCTORS


 
-- |Functions to generate a list of random constructors

constructors :: Settings -> [Constructor]
constructors s = (includeCons s) \\ (excludeCons s)

-- |Generate an infinite list of constructors

rconstructors :: RandomGen g => g -> Settings -> [Constructor]
rconstructors g s = rconstructor g s : rconstructors g1 s
    where g1 = snd $ next g 
          rconstructor g s = r g (constructors s)

-- |Constructor for case where no quanifiers have been introduced
construct :: RandomGen g => g -> Settings -> [Constructor] -> Prop
construct h s (x:y:xs) = case x of NegConstr f -> f (construct (g!!1) s (y:xs))
                                   UniConstr f -> f (r (g!!9) (variables s)) (construct1 (g!!2) s [(r (g!!9) (variables s))] (y:xs))
                                   ExiConstr f -> f (r (g!!10) (variables s)) (construct1 (g!!3) s [(r (g!!10) (variables s))] (y:xs))
                                   ConjConstr f -> constructlrb (g!!11) s f (y:xs)
                                   DisjConstr f -> constructlrb (g!!12) s f (y:xs)
                                   CondConstr f -> constructlrb (g!!13) s f (y:xs)
                                   BiconConstr f -> constructlrb (g!!14) s f (y:xs)
    where g = gens h
construct h s (x:xs) = case x of NegConstr f -> f (construct (g!!5) s (xs))
                                 UniConstr f -> f (r (g!!11) (variables s)) (construct1 (g!!6) s [(r (g!!11) (variables s))] (xs))
                                 ExiConstr f -> f (r (g!!12) (variables s)) (construct1 (g!!7) s [(r (g!!12) (variables s))] (xs))
                                 ConjConstr f -> constructlrb (g!!13) s f (xs)
                                 DisjConstr f -> constructlrb (g!!14) s f (xs)
                                 CondConstr f -> constructlrb (g!!15) s f (xs)
                                 BiconConstr f -> constructlrb (g!!16) s f (xs)
    where g = gens h
construct h s [] = ratom (g!!9) (predicats s) (constants s) "" (minArity s,maxArity s)
    where g = gens h

-- |Constructor for case where quantifiers have been introduced
construct1 :: RandomGen g => g -> Settings -> Vlets -> [Constructor] -> Prop
construct1 h s v (x:y:xs) = case x of NegConstr f -> f (construct1 (g!!1) s v (y:xs))
                                      UniConstr f -> f (r (g!!9) (variables s)) (construct1  (g!!2) s ((r (g!!9) (variables s)):v) (y:xs))
                                      ExiConstr f -> f (r (g!!10) (variables s)) (construct1 (g!!3) s ((r (g!!10) (variables s)):v) (y:xs))
                                      ConjConstr f -> constructlrb1 (g!!11) s v f (y:xs)
                                      DisjConstr f -> constructlrb1 (g!!12) s v f (y:xs)
                                      CondConstr f -> constructlrb1 (g!!13) s v f (y:xs)
                                      BiconConstr f -> constructlrb1 (g!!14) s v f (y:xs)
     where g = gens h
construct1 h s v (x:xs) = case x of NegConstr f -> f (construct1 (g!!5) s v (xs))
                                    UniConstr f -> f (r (g!!11) (variables s)) (construct1 (g!!6) s ((r (g!!11) (variables s)):v) (xs))
                                    ExiConstr f -> f (r (g!!12) (variables s)) (construct1 (g!!7) s ((r (g!!12) (variables s)):v) (xs))
                                    ConjConstr f -> constructlrb1 (g!!13) s v f (xs)
                                    DisjConstr f -> constructlrb1 (g!!14) s v f (xs)
                                    CondConstr f -> constructlrb1 (g!!15) s v f (xs)
                                    BiconConstr f -> constructlrb1 (g!!16) s v f (xs)
     where g = gens h
construct1 h s v [] = ratom (g!!9) (predicats s) (constants s) v (minArity s,minArity s)
     where g = gens h


-- |Unknown constructors
constructlrb :: RandomGen g => g -> Settings -> (Prop -> Prop -> Prop) -> [Constructor] -> Prop 
constructlrb g s f xs = let (l,r) = rsplitAt g xs in
                        f (construct g1 s l) (construct g2 s r) 
                        where (g1,g2) = split g

constructlrb1 :: RandomGen g => g -> Settings -> Vlets -> (Prop -> Prop -> Prop) -> [Constructor] -> Prop 
constructlrb1 g s v f xs = let (l,r) = rsplitAt g xs in
                           f (construct1 g1 s v l) (construct1 g2 s v r) 
                           where (g1,g2) = split g

-- |Generate an infinite list of random atomics

type Plets = [Char]
type Vlets = [Char]
type Clets = [Char]

ratoms :: RandomGen g => g -> Plets -> Clets -> Vlets -> (Int,Int) -> [Prop]
ratoms g p c v n = ratom g1 p c v n : ratoms g2 p c v n
                   where (g1,g2) = split g

-- |Random Atom
ratom :: RandomGen g => g -> Plets -> Clets -> Vlets -> (Int,Int) -> Prop
ratom g p c v n = Atomic (rpredicate g1 p) (rterms g2 c v n)
                  where (g1,g2) = split g

-- |Random Variable
rvariable :: RandomGen g => g -> Vlets -> Term
rvariable g v = Variable (r g v)

-- |Random Constant
rconstant :: RandomGen g => g -> Clets -> Term
rconstant g c = Constant (r g c)

-- |Random Predicate
rpredicate :: RandomGen g => g -> Plets -> Predicate
rpredicate g p = Predicate (r g p)

-- |Random Terms
rterms :: RandomGen g => g -> Clets -> Vlets -> (Int,Int) -> [Term]
rterms g c v n = let num = fst (randomR n g) in 
                 take num (terms g c v)

terms :: RandomGen g => g -> Clets -> Vlets -> [Term]
terms g c v = orterms g1 c v : terms  g2 c v       
              where (g1,g2) = split g
 
orterms :: RandomGen g => g -> Clets -> Vlets -> Term
orterms g c [] = let (g1,g2) = split g in
                 rconstant g2 c
orterms g c v = let test = (fst (random g)) :: Bool in 
                let (g1,g2) = split g in
                    if test
                        then rvariable g1 v  
                        else rconstant g2 c 

-- |FILTERS

-- |Is there any vacious quantification? (seems to be correct)
novac :: Prop -> Bool
novac p = not $ vac1 p

vac1 :: Prop -> Bool
vac1 (Atomic _ _) = False
vac1 (Negation p) = vac1 p
vac1 (Conjunction l r) = (vac1 l) || (vac1 r)
vac1 (Disjunction l r) = (vac1 l) || (vac1 r)
vac1 (Conditional l r) = (vac1 l) || (vac1 r)
vac1 (Biconditional l r) = (vac1 l) || (vac1 r)
vac1 (Existential x p) = (vac2 x p) || (vac1 p) 
vac1 (Universal x p) = (vac2 x p) || (vac1 p) 

vac2 :: Char -> Prop -> Bool
vac2 y (Atomic _ t) = if y `elem` (termsVariables t) 
                          then False
                          else True
vac2 y (Negation p) = (vac2 y p) 
vac2 y (Conjunction l r) = (vac2 y l) || (vac2 y r) 
vac2 y (Disjunction l r) = (vac2 y l) || (vac2 y r) 
vac2 y (Conditional l r) = (vac2 y l) || (vac2 y r) 
vac2 y (Biconditional l r) = (vac2 y l) || (vac2 y r) 
vac2 y (Existential x p) = if x == y 
                               then True
                               else vac2 y p 
vac2 y (Universal x p) = if x == y
                             then True
                             else vac2 y p 

propVariables :: Char -> Prop -> [Char]
propVariables c (Atomic (Predicate _) xs) = termsVariables xs
propVariables c (Negation l) = propVariables c l
propVariables c (Conjunction l r) = propVariables c l ++ propVariables c r
propVariables c (Disjunction l r) = propVariables c l ++ propVariables c r
propVariables c (Conditional l r) = propVariables c l ++ propVariables c r
propVariables c (Biconditional l r) = propVariables c l ++ propVariables c r
propVariables c (Universal x p) = if c == x
                                      then []
                                      else propVariables c p 
propVariables c (Existential x p) = if c == x
                                        then []
                                        else propVariables c p 

termsVariables :: [Term] -> [Char]
termsVariables [] = []
termsVariables (Variable x:xs) = x : termsVariables xs
termsVariables (x:xs) = termsVariables xs 

-- | Is the formula closed?
isclosed = not . isopen

-- | Is the formula open? (known bug: false positives)
isopen :: Prop -> Bool
isopen p = any strip (strip2 p)  

strip (Variable _) = True
strip x = False

strip2 :: Prop -> [Term]
strip2 (Atomic _ t) = t 
strip2 (Negation p) = strip2 p
strip2 (Conjunction l r) = (strip2  l) ++ (strip2  r)
strip2 (Disjunction l r) = (strip2  l) ++ (strip2  r)
strip2 (Conditional l r) = (strip2  l) ++ (strip2  r)
strip2 (Biconditional l r) = (strip2  l) ++ (strip2  r)
strip2 (Existential x p) = (strip11 [x] p) 
strip2 (Universal x p) = (strip11 [x] p) 

-- |TODO: this needs to involve [Char]. stripit will need to be changed accordingly
strip11 :: [Char] -> Prop -> [Term]
strip11 c (Atomic _ t) = stripit c t 
strip11 c (Negation p) = strip11 c p
strip11 c (Conjunction l r) = (strip11 c l) ++ (strip11 c r)
strip11 c (Disjunction l r) = (strip11 c l) ++ (strip11 c r)
strip11 c (Conditional l r) = (strip11 c l) ++ (strip11 c r)
strip11 c (Biconditional l r) = (strip11 c l) ++ (strip11 c r)
strip11 c (Existential x p) = (strip11 (x:c) p) 
strip11 c (Universal x p) = (strip11 (x:c) p) 

stripit :: [Char] -> [Term] -> [Term]
stripit (c:cs) t = stripit cs $ filter (\x -> x /= (Variable c)) t 
stripit [] t = t

handyfilter :: Prop -> Bool
handyfilter p = (\x -> (novac x)) p

-- handyfilter p = (\x -> (vac x && isclosed x)) p

-- |Pretty Printing

printProp (Atomic (Predicate x) terms) = x : printterms terms
printProp (Negation p) = "~" ++ printProp p
printProp (Conjunction l r) = "(" ++ (printProp l) ++ "&" ++ (printProp r) ++ ")" 
printProp (Disjunction l r) = "(" ++ (printProp l) ++ "v" ++ (printProp r) ++ ")"
printProp (Conditional l r) = "(" ++ (printProp l) ++ "->" ++ (printProp r) ++ ")"
printProp (Biconditional l r) = "(" ++ (printProp l) ++ "<->" ++ (printProp r) ++ ")"
printProp (Universal c p) = "@" ++ [c] ++ printProp p 
printProp (Existential c p) = "#" ++ [c] ++ printProp p
 
printterms (Variable x:xs) = x: printterms xs
printterms (Constant x:xs) = x: printterms xs
printterms [] = [] 


