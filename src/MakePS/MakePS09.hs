{-# LANGUAGE OverloadedStrings #-}

module MakePS.MakePS09 (mkps09g, mkps09string) where


import Text.LaTeX
import Text.LaTeX.Base.Commands
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.Geometry
import Text.LaTeX.Packages.Inputenc   -- usepackage [utf8] inputenc
import Text.LaTeX.Base.Pretty         -- prettyLaTeX
import Text.LaTeX.Packages.Trees.Qtree
import Text.LaTeX.Packages.AMSSymb
import Text.LaTeX.Base.Math


import System.Random

import Printing.LaTeXGPLIProps (printprops,printarg) 
import Printing.LaTeXGPLITrees (printtree) 
import Printing.LaTeXGPLIModel (printmodels)

import Settings.GPLISettings

import Random.GPLIprop (gpltautg,gplsatg,prepforvalidity,prepfortaut)
import Trees.GPLItrees (mktree, getmodels)

-- |GENERAL DOCUMENT BUILDING FUNCTIONS

-- | just give me a string man!
mkps09string :: IO (String, String)
mkps09string = do
       g <- newStdGen    -- get random generator
       let (num,_) = next g  -- use it to get a random number
       let seed = mkStdGen num
       let (g1,g2) = split seed        
       let (q1q,q1a) = getq1g g1
       let (q2q,q2a) = getq2g g2
       let questionstring = prettyLaTeX (ps09q (q1q,q2q) num)
       let answerstring = prettyLaTeX (ps09a (q1q,q1a) (q2q,q2a) num)
       return (questionstring,answerstring)

-- |function to render questions and answers to .tex file
mkps09g :: RandomGen g => g -> Int -> IO ()
mkps09g g n = do
         let (q1q,q1a) = getq1g g1
         let (q2q,q2a) = getq2g g2
         renderFile ("ps09" ++ "-" ++ (show n) ++ "q.tex") (ps09q (q1q,q2q) n) -- render questions to tex
         renderFile ("ps09" ++ "-" ++ (show n) ++ "a.tex") (ps09a (q1q,q1a) (q2q,q2a) n) -- render answers to tex
        where (g1,g2) = split g
-- |here we get the random prop(s), make the tree, return the LaTeX versions

getq1g :: RandomGen g => g ->  (LaTeX,LaTeX)
getq1g g = let p = gpltautg g settingPS09a in
           let t = mktree (prepfortaut p) in
           (printprops p, printtree t)

getq2g :: RandomGen g => g ->  (LaTeX,LaTeX)
getq2g g = let p = gplsatg g settingPS09b in
           let t = mktree p in
           (printprops p, (printtree t <> quote (printmodels $ getmodels t)))

-- |document preamble

-- |preamble for questions
ps09pq :: Int -> LaTeX 
ps09pq n = docSettings <> title "Problem Set 09: GPL Trees (Questions)" <> author "" <> date (fromString $ "#" ++ show n)

-- |preamble for answers
ps09pa :: Int -> LaTeX 
ps09pa n = docSettings <> title "Problem Set 09: GPL Trees (Answers)" <> author "" <> date (fromString $ "#" ++ show n)

-- |shared document settings
docSettings :: LaTeX
docSettings = documentclass [] article 
            <> usepackage [] amssymb 
            <> usepackage [utf8] inputenc 
            <> usepackage [] qtree 
            <> importGeometry [GWidth (Cm 16)]

-- |final latex document to render

-- |only questions
ps09q :: (LaTeX,LaTeX) -> Int -> LaTeX
ps09q x n = ps09pq n <> document (maketitle <> questions x)

-- |with answers
ps09a :: (LaTeX,LaTeX) -> (LaTeX,LaTeX) -> Int -> LaTeX
ps09a x y n = ps09pa n <> document (maketitle <> answers x y)

-- |DOCUMENT BODY

-- |shared

-- | the text of q1
q1text :: LaTeX
q1text = item Nothing <> "Use a tree to test whether the following is a tautology. If it is not, then read a countermodel off the tree."

-- | the text of q2
q2text :: LaTeX
q2text = item Nothing <> "Use a tree to test whether the following propositions are jointly satisfiable. If they are not, then read a countermodel off the tree."

-- |template for just the questions

-- |question 1 question template, expects the question in LaTeX
q1qtemp :: LaTeX -> LaTeX
q1qtemp q = item Nothing <> q

-- |question 2 question template
q2qtemp :: LaTeX -> LaTeX
q2qtemp q = item Nothing <> q

-- |template for questions and answers

-- |question 1 question and answer template
q1atemp :: LaTeX -> LaTeX -> LaTeX
q1atemp q a = item Nothing <> q <> item Nothing <> "Answer:" <> quote a

-- |question 2 question and answer template
q2atemp :: LaTeX -> LaTeX -> LaTeX
q2atemp q a = item Nothing <> q <> item Nothing <> "Answer:" <> quote a

-- |only questions template -- expects each question in order
questions :: (LaTeX,LaTeX) -> LaTeX
questions (x,y) = enumerate (q1text <> enumerate (q1qtemp x) <> q2text <> enumerate (q2qtemp y))

-- |questions and answers template -- expects question and answer pairs
answers :: (LaTeX,LaTeX) -> (LaTeX,LaTeX) -> LaTeX
answers (xq,xa) (yq,ya) = enumerate (q1text <> enumerate (q1atemp xq xa) <> q2text <> enumerate (q2atemp yq ya))




