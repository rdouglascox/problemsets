module Random.PLprops where

import System.Random
import Data.PLprop
import Tables.Tables (getbasics, taut, sat, norow, norow')
import Data.List
import Printing.PLprop
import Trees.PLtrees

import Settings.PLSettings

-- non IO versions

plcontrariesg :: RandomGen g => g -> Settings -> [Prop]
plcontrariesg gen s = do
                     head $ take 1 $ rcont' gen s

plvalidg :: RandomGen g => g -> Settings -> [Prop]
plvalidg gen s = do
          head $ take 1 $ rvalid gen s
              
                                    
plvalid2g :: RandomGen g => g -> Settings -> [Prop]
plvalid2g gen s = do
            head $ take 1 $ rvalid gen s


plequivsg :: RandomGen g => g -> Settings -> [Prop]
plequivsg gen s = do
               head $ take 1 $ requivs gen s
                    where localSettings = dSettings {numProps = 2, basics = "LM", minBranchSet = 0}

-- for trees

plcontraries :: IO ([Prop])
plcontraries = do
               gen <- newStdGen
               let prop = head $ take 1 $ rcont' gen dSettings
               return (prop)
 
 
prepfc :: [Prop] -> [Prop]
prepfc [l,r] = [(Negation (Conjunction l r))]

unfc :: [Prop] -> [Prop]
unfc [(Negation (Conjunction l r))] = [l,r]

rcont :: RandomGen g => g -> Settings -> [[Prop]]
rcont gen s = filter (allpathsclosed . mktree) $ map unfc $ filter (allpathsclosed .mktree) $ filter (superfilter s) $ map prepfc $ nrprops gen s 


rcont' :: RandomGen g => g -> Settings -> [[Prop]]
rcont' gen s = filter (allpathsclosed . mktree) $ filter (superfilter s) $ map unfc $ filter (superfilter s) $ map prepfc $ nrprops gen s 

plvalid :: IO ([Prop])
plvalid = do
          gen <- newStdGen
          let prop = head $ take 1 $ rvalid gen localSettings
          return (prop)
    where localSettings = dSettings {numProps = 3,minBranchSet = 4,maxBranchSet =5,maxConstr = 5}


plvalid2 :: IO ([Prop])
plvalid2 = do
           gen <- newStdGen
           let prop = head $ take 1 $ rvalid gen localSettings
           return (prop)
     where localSettings = dSettings {numProps = 3
                                     ,basics = "XYZ"
                                     ,minConstr = 2
                                     ,maxConstr = 3
                                     }

rvalid :: RandomGen g => g -> Settings -> [[Prop]]
rvalid gen s = map unvalid $ filter (allpathsclosed .mktree) $ filter (superfilter s) $ map prepforvalidity $ nrprops gen s

prepforvalidity :: [Prop] -> [Prop]
prepforvalidity xs = reverse (tail $ reverse xs) ++ [(Negation (head $ reverse xs))] 

unvalid :: [Prop] -> [Prop]
unvalid (x:y:(Negation p):[]) = x : y : p : []

-- for tables (use validity again)

plequivs :: IO ([Prop])
plequivs = do
           gen <- newStdGen
           let prop = head $ take 1 $ requivs gen localSettings
           return (prop)
    where localSettings = dSettings {numProps = 2, basics = "LM", minBranchSet = 0}

requivs :: RandomGen g => g -> Settings -> [[Prop]]
requivs gen s = map unequiv $ filter (allpathsclosed . mktree) $ filter (superfilter s) $ map prepforequiv $ nrprops gen s 

prepforequiv :: [Prop] -> [Prop]
prepforequiv [l,r] = [(Negation (Biconditional l r))]

unequiv :: [Prop] -> [Prop]
unequiv [(Negation (Biconditional l r))] = [l,r]


superfilter :: Settings -> [Prop] -> Bool
superfilter s ps = (minBranch (minBranchSet s) tree) && (maxBranch (maxBranchSet s) tree) && (minPath (minPathSet s) tree) && (maxPath (maxPathSet s) tree)
    where tree = mktree ps


-- HELPERS

r :: RandomGen g => g -> [a] -> a
r g x = x!!(fst(randomR(0,((length x)) -1) g))

gens :: RandomGen g => g -> [g]
gens g = g : gens (snd $ next g) 

rsplitAt :: RandomGen g => g -> [a] -> ([a],[a])
rsplitAt g xs = let y = (fst $ randomR (0,(length xs)) g) in
    splitAt y xs

-- |Functions to construct a proposition from a list of random constructors



constructors :: Settings -> [Constructor]
constructors s = (includeCons s) \\ (excludeCons s)

rconstructors :: RandomGen g => g -> Settings -> [Constructor]
rconstructors g s = rconstructor g s : rconstructors g1 s
    where g1 = snd $ next g
          rconstructor g s = r g (constructors s)

construct :: RandomGen g => g -> Settings -> [Constructor] -> Prop
construct h s (x:y:xs) = case x of NegConstr f -> f (construct (g!!1) s (y:xs))
                                   ConjConstr f -> constructlr (g!!2) s f (y:xs)
                                   DisjConstr f -> constructlr (g!!3) s f (y:xs)
                                   CondConstr f -> constructlr (g!!4) s f (y:xs)
                                   BiconConstr f -> constructlr (g!!5) s f (y:xs)
    where g = gens h
construct h s (x:xs) = case x of NegConstr f -> f (construct (g!!6) s (xs))
                                 ConjConstr f -> constructlr (g!!7) s f (xs)
                                 DisjConstr f -> constructlr (g!!8) s f (xs)
                                 CondConstr f -> constructlr (g!!9) s f (xs)
                                 BiconConstr f -> constructlr (g!!10) s f (xs)
    where g = gens h
construct h s [] = rbasic' (g!!11) s 
    where g = gens h

constructlr :: RandomGen g => g -> Settings -> (Prop -> Prop -> Prop) -> [Constructor] -> Prop
constructlr g s f xs = let (l,r) = rsplitAt g xs in
                       f (construct g1 s l) (construct g2 s r)
                       where (g1,g2) = split g

rbasic' :: RandomGen g => g -> Settings -> Prop
rbasic' g s = Basic [(r g (basics s))]

rprops' :: RandomGen g => g -> Settings -> [Prop]
rprops' g s = construct g1 s (take (r g4 [(minConstr s)..(maxConstr s)]) $ rconstructors g2 s) : rprops' g3 s
    where (g1,g2) = split g
          (g3,g4) = split g2  

nrprops :: RandomGen g => g -> Settings -> [[Prop]]
nrprops g s = chop (numProps s) (rprops' g s)
    where chop m xs = take m xs : chop m (drop m xs)

-- RANDOM PROPOSITIONS

rbasic :: RandomGen g => g -> String-> Prop
rbasic g x = Basic [(r g x)]

rprop :: RandomGen g => g -> String -> Prop
rprop g x = r ((gens g)!!1) [rbasic ((gens g)!!2) x
                          , Negation (rprop ((gens g)!!3) x)
                          , Conjunction (rprop ((gens g)!!4) x) (rprop ((gens g!!5)) x)
                          , Disjunction (rprop ((gens g)!!6) x) (rprop ((gens g!!7)) x)                          
                          , Conditional (rprop ((gens g)!!8) x) (rprop ((gens g!!9)) x)
                          , Biconditional (rprop ((gens g)!!10) x) (rprop ((gens g!!11)) x)
                          , rbasic ((gens g)!!12) x                           
                          , rbasic ((gens g)!!13) x
                          , rbasic ((gens g)!!14) x

                          ]

-- generate a list of random props
 
rprops :: RandomGen g => g -> String -> [Prop]
rprops g x = rprop g x : rprops g1 x
    where g1 = snd $ next g 

-- generate a list of lists consisting of one random prop

uprops :: RandomGen g => g -> String -> [[Prop]]
uprops g x = [[rprop g x]] ++ uprops g1 x
    where g1 = snd $ next g 

-- generate a list of lists consisting of two random props

dprops :: RandomGen g => g -> String -> [[Prop]]
dprops g x = [[rprop g1 x, rprop g2 x]] ++ dprops g3 x
    where g1 = snd $ next g
          g2 = snd $ next g1
          g3 = snd $ next g2 

-- generate a list of lists consisting of three random props

tprops :: RandomGen g => g -> String -> [[Prop]]
tprops g x = [[rprop g1 x, rprop g2 x, rprop g3 x]] ++ tprops g4 x
    where g1 = snd $ next g
          g2 = snd $ next g1
          g3 = snd $ next g2 
          g4 = snd $ next g3

-- generate a list of lists consisting of four random props

qprops :: RandomGen g => g -> String -> [[Prop]]
qprops g x = [[rprop g1 x, rprop g2 x, rprop g3 x, rprop g4 x]] ++ qprops g5 x
    where g1 = snd $ next g
          g2 = snd $ next g1
          g3 = snd $ next g2 
          g4 = snd $ next g3
          g5 = snd $ next g4



-- FILTERS

anyfilt :: [(a -> Bool)] -> (a -> Bool) 
anyfilt fns = \el -> any (\fn -> fn el) fns

allfilt :: [(a -> Bool)] -> (a -> Bool)
allfilt fns = \el -> all (\fn -> fn el) fns

-- Syntactic Filters

hasnbinaries :: Int -> Prop -> Bool
hasnbinaries n p = if n == (nbinary p) then True else False 

maxbinaries :: Int -> Prop -> Bool
maxbinaries n p = if n >= (nbinary p) then True else False

maxbinaries' :: Int -> [Prop] -> Bool
maxbinaries' n ps = and $ map (maxbinaries n) ps

nbinary :: Prop -> Int
nbinary (Basic x) = 0
nbinary (Negation p) = nbinary p
nbinary (Conjunction l r) = 1 + (nbinary l) + (nbinary r) 
nbinary (Disjunction l r) = 1 + (nbinary l) + (nbinary r) 
nbinary (Conditional l r) = 1 + (nbinary l) + (nbinary r)
nbinary (Biconditional l r) = 1 + (nbinary l) + (nbinary r)

hasnbasics :: Int -> Prop -> Bool
hasnbasics n p = if n == (nbasic p) then True else False 

hasnbasics' :: Int -> [Prop] -> Bool
hasnbasics' n ps = if and (map (\p -> (n == (nbasic p))) ps) then True else False 

nbasic :: Prop -> Int
nbasic (Basic x) = 1
nbasic (Negation p) = nbasic p
nbasic (Conjunction l r) = (nbasic l) + (nbasic r) 
nbasic (Disjunction l r) = (nbasic l) + (nbasic r) 
nbasic (Conditional l r) = (nbasic l) + (nbasic r)
nbasic (Biconditional l r) = (nbasic l) + (nbasic r)

hasnnegs :: Int -> Prop -> Bool
hasnnegs n p = if n == (nneg p) then True else False 

nneg :: Prop -> Int
nneg (Basic x) = 0
nneg (Negation p) = 1 + nneg p
nneg (Conjunction l r) = (nneg l) + (nneg r) 
nneg (Disjunction l r) = (nneg l) + (nneg r) 
nneg (Conditional l r) = (nneg l) + (nneg r)
nneg (Biconditional l r) = (nneg l) + (nneg r)

hasnnegsnbins :: Int -> Int -> Prop -> Bool
hasnnegsnbins x y p = if x == (nneg p) && y == (nbinary p) then True else False  

hasndistbasics :: Int -> Prop -> Bool
hasndistbasics n p = if n == (nubbasics p) then True else False

hasndistbasics' :: Int -> [Prop] -> Bool
hasndistbasics' n ps = and $ map (hasndistbasics n) ps

nubbasics :: Prop -> Int
nubbasics p = length (nub $ getbasics p)

-- this is perhaps the most helpful it takes the number of negations, number of binaries, and the number of distinct basics. this can be used for tables.  

filterprops :: Int -> Int -> Int -> Prop -> Bool
filterprops x y z p = if x == (nneg p) && y == (nbinary p) && z == (nubbasics p) then True else False  

-- Semantic Filters

isSat :: Prop -> Bool
isSat p = if sat [p] then True else False

areSat :: [Prop] -> Bool
areSat ps = if sat ps then True else False 

isTaut :: Prop -> Bool
isTaut p = if taut [p] then True else False   

areTaut :: [Prop] -> Bool
areTaut ps = if taut ps then True else False

isValid :: [Prop] -> Bool
isValid = norow

isValid' :: [Prop] -> Bool
isValid' = norow'

-- Tree Filters

hasnbranches :: Int -> [Prop] -> Bool
hasnbranches n ps = if nbranches (mktree ps) == n then True else False




