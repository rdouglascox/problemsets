{-# LANGUAGE OverloadedStrings #-}

module MakePS.MakePS08 (mkps08g, mkps08string) where


import Text.LaTeX
import Text.LaTeX.Base.Commands
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.Geometry
import Text.LaTeX.Packages.Inputenc
import Text.LaTeX.Base.Pretty
import Text.LaTeX.Packages.Trees.Qtree
import Text.LaTeX.Packages.AMSSymb
import Text.LaTeX.Base.Math


import Printing.LaTeXGPLIProps (printprops,printarg)
import Printing.LaTeXGPLITrees (printtree)
import Printing.LaTeXGPLIModel (printmodels,printmodellns)

import System.Random

import Settings.GPLISettings

import Random.GPLIprop (gplsatg, mplsatg)

import Trees.GPLItrees ( mktree, getmodels )

import Models.Evaluator (meval)
import Random.Models (rmodel,rmodelg)

-- | just give me a string man!
mkps08string :: IO (String, String)
mkps08string = do
       g <- newStdGen    -- get random generator
       let (num,_) = next g  -- use it to get a random number
       let seed = mkStdGen num
       let (g1,g2) = split seed        
       let (q1q,q1a) = getq1g g1
       let (q2q,q2a) = getq2g g2
       let questionstring = prettyLaTeX (ps08q (q1q,q2q) num)
       let answerstring = prettyLaTeX (ps08a (q1q,q1a) (q2q,q2a) num)
       return (questionstring,answerstring)


-- |function to render questions and answers to .tex file
mkps08g :: RandomGen g => g -> Int -> IO ()
mkps08g g n = do
           let (q1q,q1a) = getq1g g1
           let (q2q,q2a) = getq2g g2
           renderFile ("ps08" ++ "-" ++ (show n) ++ "q.tex") (ps08q (q1q,q2q) n) -- render questions to tex
           renderFile ("ps08" ++ "-" ++ (show n) ++ "a.tex") (ps08a (q1q,q1a) (q2q,q2a) n) -- render answers to tex
           where (g1,g2) = split g

-- |here we get the random prop(s), make the tree, return the LaTeX versions

getq1g :: RandomGen g => g ->  (LaTeX,LaTeX)
getq1g g = let p = mplsatg g settingPS08a in
           let m = rmodelg g p in
           let a = meval p m in
           (printprops p <> newline <> newline <> printmodellns m, fromString (show a))

getq2g :: RandomGen g => g -> (LaTeX,LaTeX)
getq2g g = let p = gplsatg g settingPS08b in
           let m = rmodelg g p in
           let a = meval p m in
           (printprops p <> newline <> newline <> printmodellns m, fromString (show a))


-- |document preamble

-- |preamble for questions
ps08pq :: Int -> LaTeX
ps08pq n = docSettings <> title "Problem Set 08: MPL Models (Questions)" <> author "" <> date (fromString $ "#" ++ show n)

-- |preamble for answers
ps08pa :: Int -> LaTeX
ps08pa n = docSettings <> title "Problem Set 08: MPL Models (Answers)" <> author "" <> date (fromString $ "#" ++ show n)

-- |shared document settings
docSettings :: LaTeX
docSettings = documentclass [] article
            <> usepackage [] amssymb
            <> usepackage [utf8] inputenc
            <> usepackage [] qtree
            <> importGeometry [GWidth (Cm 18)]

-- |final latex document to render

-- |only questions
ps08q :: (LaTeX,LaTeX) -> Int -> LaTeX
ps08q x n = ps08pq n <> document (maketitle <> questions x)

-- |with answers
ps08a :: (LaTeX,LaTeX) -> (LaTeX,LaTeX) -> Int -> LaTeX
ps08a x y n = ps08pa n <> document (maketitle <> answers x y)

-- |DOCUMENT BODY

-- |shared

-- | the text of q1
q1text :: LaTeX
q1text = item Nothing <> "Is the following proposition true or false in the given model? Briefly explain your anwser."

-- | the text of q2
q2text :: LaTeX
q2text = item Nothing <> "Is the following proposition true or false in the given model? Briefly explain your answer."

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




