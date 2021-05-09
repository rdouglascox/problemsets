{-# LANGUAGE OverloadedStrings #-}

module ProblemSet02.MakePS02 (mkps02) where

-- |HaLaTeX imports 

import Text.LaTeX
import Text.LaTeX.Base.Commands
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.Geometry
import Text.LaTeX.Packages.Inputenc   -- usepackage [utf8] inputenc
import Text.LaTeX.Base.Pretty         -- prettyLaTeX
import Text.LaTeX.Packages.Trees.Qtree
import Text.LaTeX.Packages.AMSSymb
import Text.LaTeX.Base.Math

-- |Imports for questions and answers

-- |printing functions

import Printing.LaTeXPLProps
import Printing.LaTeXTables
import Tables.Tables

-- |random prop functions

import Random.PLprops

-- |tree building


-- |GENERAL DOCUMENT BUILDING FUNCTIONS

-- |function to render questions and answers to .tex file
mkps02 :: IO ()
mkps02 = do
         (q1q,q1a) <- getq1 
         (q2q,q2a) <- getq2
         renderFile "ps02q.tex" (ps02q (q1q,q2q)) -- render questions to tex
         renderFile "ps02a.tex" (ps02a (q1q,q1a) (q2q,q2a)) -- render answers to tex

-- |here we get the random prop(s), make the tree, return the LaTeX versions

getq1 :: IO (LaTeX,LaTeX)
getq1 = do
        p <- plequivs
        let t = makeRawTable p
        return (printprops p, makeTable t)

getq2 :: IO (LaTeX,LaTeX)
getq2 = do
        p <- plvalid2
        let t = makeRawTable p
        return (printarg p, makeTable t)

-- |document preamble

-- |preamble for questions
ps02pq :: LaTeX 
ps02pq = docSettings <> title "Problem Set 02: PL Tables (Questions)" <> author "" <> date ""

-- |preamble for answers
ps02pa :: LaTeX 
ps02pa = docSettings <> title "Problem Set 02: PL Tables (Answers)" <> author "" <> date ""

-- |shared document settings
docSettings :: LaTeX
docSettings = documentclass [] article 
            <> usepackage [] amssymb 
            <> usepackage [utf8] inputenc 
            <> usepackage [] qtree 
            <> importGeometry [GWidth (Cm 18)]

-- |final latex document to render

-- |only questions
ps02q :: (LaTeX,LaTeX) -> LaTeX
ps02q x = ps02pq <> document (maketitle <> questions x)

-- |with answers
ps02a :: (LaTeX,LaTeX) -> (LaTeX,LaTeX) -> LaTeX
ps02a x y = ps02pa <> document (maketitle <> answers x y)

-- |DOCUMENT BODY

-- |shared

-- | the text of q1
q1text :: LaTeX
q1text = item Nothing <> "Use a truth table to test whether the following propositions are equivalient."

-- | the text of q2
q2text :: LaTeX
q2text = item Nothing <> "Use a truth table to test whether the following argument is valid."

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




