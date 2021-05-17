{-# LANGUAGE OverloadedStrings #-}

module MakePS.MakePS01 (mkps01g) where

import Text.LaTeX
import Text.LaTeX.Base.Commands
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.Geometry
import Text.LaTeX.Packages.Inputenc   -- usepackage [utf8] inputenc
import Text.LaTeX.Base.Pretty         -- prettyLaTeX
import Text.LaTeX.Packages.Trees.Qtree
import Text.LaTeX.Packages.AMSSymb
import Text.LaTeX.Base.Math

import System.Random ( RandomGen(split) )

import Translations.RandomSentences

-- |GENERAL DOCUMENT BUILDING FUNCTIONS

-- |function to render questions and answers to .tex file
mkps01g :: RandomGen g => g -> Int -> IO ()
mkps01g g n  = do
         let (q1q,q1a) = getq1g g1
         let (q2q,q2a) = getq2g g2
         renderFile ("ps01" ++ "-" ++ (show n) ++ "q.tex") (ps01q (q1q,q2q) n) -- render questions to tex
         renderFile ("ps01" ++ "-" ++ (show n) ++ "a.tex") (ps01a (q1q,q1a) (q2q,q2a) n) -- render answers to tex
              where (g1,g2) = split g

-- |here we get the random prop(s), make the tree, return the LaTeX versions

getq1g :: RandomGen g => g -> (LaTeX,LaTeX)
getq1g = pltranslationg 

getq2g :: RandomGen g => g -> (LaTeX,LaTeX)
getq2g = pltranslationg


-- |document preamble

-- |preamble for questions
ps01pq :: Int -> LaTeX 
ps01pq n = docSettings <> title "Problem Set 01: PL Translations (Questions)" <> author "" <> date (fromString $ "#" ++ show n)

-- |preamble for answers
ps01pa :: Int -> LaTeX 
ps01pa n = docSettings <> title "Problem Set 01: PL Translations (Answers)" <> author "" <> date (fromString $ "#" ++ show n)

-- |shared document settings
docSettings :: LaTeX
docSettings = documentclass [] article 
            <> usepackage [] amssymb 
            <> usepackage [utf8] inputenc 
            <> usepackage [] qtree 
            <> importGeometry [GWidth (Cm 18)]

-- |final latex document to render

-- |only questions
ps01q :: (LaTeX,LaTeX) -> Int ->  LaTeX
ps01q x n = ps01pq n <> document (maketitle <> questions x)

-- |with answers
ps01a :: (LaTeX,LaTeX) -> (LaTeX,LaTeX) -> Int -> LaTeX
ps01a x y n = ps01pa n <> document (maketitle <> answers x y)

-- |DOCUMENT BODY

-- |shared

-- | the text of q1
q1text :: LaTeX
q1text = item Nothing <> "Translate the following into PL. Provide a glossary for your translation."

-- | the text of q2
q2text :: LaTeX
q2text = item Nothing <> "Translate the following into PL. Provide a glossary for your translation."

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




