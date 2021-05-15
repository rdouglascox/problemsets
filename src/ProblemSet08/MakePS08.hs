{-# LANGUAGE OverloadedStrings #-}

module ProblemSet08.MakePS08 (mkps08,mkps08g) where


import Text.LaTeX
import Text.LaTeX.Base.Commands
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.Geometry
import Text.LaTeX.Packages.Inputenc   -- usepackage [utf8] inputenc
import Text.LaTeX.Base.Pretty         -- prettyLaTeX
import Text.LaTeX.Packages.Trees.Qtree
import Text.LaTeX.Packages.AMSSymb
import Text.LaTeX.Base.Math


import Printing.LaTeXGPLIProps (printprops,printarg) 
import Printing.LaTeXGPLITrees (printtree) 
import Printing.LaTeXGPLIModel (printmodels,printmodellns)

import System.Random

import Random.GPLIprop (gplsat1, mplsat1, gplsat1g, mplsat1g)

import Trees.GPLItrees (mktree)
import Trees.GPLItrees (getmodels)

import Models.Evaluator (meval) 
import Random.Models (rmodel,rmodelg)


-- |function to render questions and answers to .tex file
mkps08g :: RandomGen g => g -> IO ()
mkps08g g = do
           let (q1q,q1a) = getq1g g1
           let (q2q,q2a) = getq2g g2
           renderFile "ps08q.tex" (ps08q (q1q,q2q)) -- render questions to tex
           renderFile "ps08a.tex" (ps08a (q1q,q1a) (q2q,q2a)) -- render answers to tex
           where (g1,g2) = split g

-- |here we get the random prop(s), make the tree, return the LaTeX versions

getq1g :: RandomGen g => g ->  (LaTeX,LaTeX)
getq1g g = let p = mplsat1g g in
           let m = rmodelg g p in
           let a = meval p m in
           (printprops p <> newline <> newline <> printmodellns m, fromString (show a))

getq2g :: RandomGen g => g -> (LaTeX,LaTeX)
getq2g g = let p = gplsat1g g in
           let m = rmodelg g p in
           let a = meval p m in
           (printprops p <> newline <> newline <> printmodellns m, fromString (show a))









-- |GENERAL DOCUMENT BUILDING FUNCTIONS

-- |function to render questions and answers to .tex file
mkps08 :: IO ()
mkps08 = do
         (q1q,q1a) <- getq1 
         (q2q,q2a) <- getq2
         renderFile "ps08q.tex" (ps08q (q1q,q2q)) -- render questions to tex
         renderFile "ps08a.tex" (ps08a (q1q,q1a) (q2q,q2a)) -- render answers to tex

-- |here we get the random prop(s), make the tree, return the LaTeX versions

getq1 :: IO (LaTeX,LaTeX)
getq1 = do
        p <- mplsat1
        m <- rmodel p
        let a = meval p m
        return (printprops p <> newline <> newline <> printmodellns m, fromString (show a))

getq2 :: IO (LaTeX,LaTeX)
getq2 = do
        p <- gplsat1
        m <- rmodel p
        let a = meval p m
        return (printprops p <> newline <> newline <> printmodellns m, fromString (show a))

-- |document preamble

-- |preamble for questions
ps08pq :: LaTeX 
ps08pq = docSettings <> title "Problem Set 08: MPL Models (Questions)" <> author "" <> date ""

-- |preamble for answers
ps08pa :: LaTeX 
ps08pa = docSettings <> title "Problem Set 08: MPL Models (Answers)" <> author "" <> date ""

-- |shared document settings
docSettings :: LaTeX
docSettings = documentclass [] article 
            <> usepackage [] amssymb 
            <> usepackage [utf8] inputenc 
            <> usepackage [] qtree 
            <> importGeometry [GWidth (Cm 18)]

-- |final latex document to render

-- |only questions
ps08q :: (LaTeX,LaTeX) -> LaTeX
ps08q x = ps08pq <> document (maketitle <> questions x)

-- |with answers
ps08a :: (LaTeX,LaTeX) -> (LaTeX,LaTeX) -> LaTeX
ps08a x y = ps08pa <> document (maketitle <> answers x y)

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




