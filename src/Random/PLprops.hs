module Random.PLprops where

import System.Random
import Data.PLprop
import Tables.Tables (getbasics, taut, sat, norow, norow')
import Data.List
import Printing.PLprop
import qualified Trees.PLtreesNew as NT
import Trees.PLtrees

import Settings.PLSettings

rPL :: RandomGen g => g -> Settings -> [Prop]
rPL gen s = head $ take 1 $ nrprops gen s

-- functions to generate random lists of lists of propositions, depending on
-- settings for basics to choose from, the number of connectives to draw from,
-- the list of connectives to draw from, and the number of propositions to include
-- in each list

r :: RandomGen g => g -> [a] -> a
r g x = x!!fst(randomR(0,length x -1) g)

gens :: RandomGen g => g -> [g]
gens g = g : gens (snd $ split g)

rsplitAt :: RandomGen g => g -> [a] -> ([a],[a])
rsplitAt g xs = let y = (fst $ randomR (0,length xs) g) in
    splitAt y xs

-- | get a list of constructors by taking the difference of the set of constructors
-- | to include and the list of constructors to exclude
constructors :: Settings -> [Constructor]
constructors s = includeCons s \\ excludeCons s

-- | get an infinite list of random constructors to build the proposition out of
rconstructors :: RandomGen g => g -> Settings -> [Constructor]
rconstructors g s = rconstructor g s : rconstructors g1 s
    where g1 = snd $ next g
          rconstructor g s = r g (constructors s)

-- | construct a proposition for a list of constructors
construct :: RandomGen g => g -> Settings -> [Constructor] -> Prop
construct h s (x:y:xs) = case x of NegConstr f -> f (construct (g!!1) s (y:xs))
                                   ConjConstr f -> constructlr (g!!2) s f (y:xs)
                                   DisjConstr f -> constructlr (g!!3) s f (y:xs)
                                   CondConstr f -> constructlr (g!!4) s f (y:xs)
                                   BiconConstr f -> constructlr (g!!5) s f (y:xs)
    where g = gens h
construct h s (x:xs) = case x of NegConstr f -> f (construct (g!!6) s xs)
                                 ConjConstr f -> constructlr (g!!7) s f xs
                                 DisjConstr f -> constructlr (g!!8) s f xs
                                 CondConstr f -> constructlr (g!!9) s f xs
                                 BiconConstr f -> constructlr (g!!10) s f xs
    where g = gens h
construct h s [] = rbasic (g!!11) s
    where g = gens h
          rbasic g s = Basic [r g (basics s)]

-- | a helper function 
constructlr :: RandomGen g => g -> Settings -> (Prop -> Prop -> Prop) -> [Constructor] -> Prop
constructlr g s f xs = let (l,r) = rsplitAt g xs in
                       f (construct g1 s l) (construct g2 s r)
                       where (g1,g2) = split g

-- | generate an infinite random list of propositions given some setting
rprops' :: RandomGen g => g -> Settings -> [Prop]
rprops' g s = construct g1 s (take (r g4 [(minConstr s)..(maxConstr s)]) $ rconstructors g2 s) : rprops' g3 s
    where (g1,g2) = split g
          (g3,g4) = split g2

-- | generate an infinite random list of lists of propositions given some setting
nrprops :: RandomGen g => g -> Settings -> [[Prop]]
nrprops g s = chop (numProps s) (rprops' g s)
    where chop m xs = take m xs : chop m (drop m xs)

-- functions to filter lists of lists of propositions to meet further conditions imposed
-- by the relevant settings. these include filtering out unsatisfiable sets of propositions,
-- filtering out sets of propositions which do not make for valid arguments, and so on, as 
-- well as filtering out sets of propositions which generate truth trees which are simply
-- too big (either in terms of number of branches or in terms of length of paths)

plcontrariesg :: RandomGen g => g -> Settings -> [Prop]
plcontrariesg gen s =
                     head $ take 1 $ rcont' gen s

plvalidg :: RandomGen g => g -> Settings -> [Prop]
plvalidg gen s =
          head $ take 1 $ rvalid gen s

plequivsg :: RandomGen g => g -> Settings -> [Prop]
plequivsg gen s =
               head $ take 1 $ requivs gen s

justanrprop :: RandomGen g => g -> Settings -> Prop
justanrprop gen s =
               head $ take 1 $ rprops' gen s

-- for trees

prepfc :: [Prop] -> [Prop]
prepfc [l,r] = [(Conjunction (Negation l) (Negation r))]

unfc :: [Prop] -> [Prop]
unfc [(Conjunction (Negation l) (Negation r))] = [l,r]

rcont :: RandomGen g => g -> Settings -> [[Prop]]
rcont gen s = filter areNotSat $ map unfc $ filter areNotSat $ filter (superfilter s) $ map prepfc $ nrprops gen s

rcont' :: RandomGen g => g -> Settings -> [[Prop]]
rcont' gen s =  filter (superfilter s) $ map unfc $ filter (superfilter s) $ map prepfc $ filter areNotSat $ nrprops gen s

rvalid :: RandomGen g => g -> Settings -> [[Prop]]
rvalid gen s = map unvalid $ filter (superfilter s) $ filter areNotSat $ map prepforvalidity $ nrprops gen s

prepforvalidity :: [Prop] -> [Prop]
prepforvalidity xs = init xs ++ [Negation (last xs)]

unvalid :: [Prop] -> [Prop]
unvalid [x, y, Negation p] = [x, y, p]

requivs :: RandomGen g => g -> Settings -> [[Prop]]
requivs gen s = map unequiv $ filter (allpathsclosed . NT.mktree) $ filter (superfilter s) $ map prepforequiv $ nrprops gen s

prepforequiv :: [Prop] -> [Prop]
prepforequiv [l,r] = [Negation (Biconditional l r)]

prepforequiv' :: Prop -> Prop -> [Prop]
prepforequiv' l r = [Negation (Biconditional l r)]

unequiv :: [Prop] -> [Prop]
unequiv [Negation (Biconditional l r)] = [l,r]

superfilter :: Settings -> [Prop] -> Bool
superfilter s ps = minBranch (minBranchSet s) tree && maxBranch (maxBranchSet s) tree && minPath (minPathSet s) tree && maxPath (maxPathSet s) tree
    where tree = NT.mktree ps

-- Semantic Filters

isSat :: Prop -> Bool
isSat p = sat [p]

areSat :: [Prop] -> Bool
areSat = sat

areNotSat :: [Prop] -> Bool 
areNotSat = not . sat

isTaut :: Prop -> Bool
isTaut p = taut [p]

areTaut :: [Prop] -> Bool
areTaut = taut

-- | is valid, basic case
isValid :: [Prop] -> Bool
isValid = norow

-- | is valid but conclusion is not true on every row
isValid' :: [Prop] -> Bool
isValid' = norow'



