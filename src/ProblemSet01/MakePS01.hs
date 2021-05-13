{-# LANGUAGE OverloadedStrings #-}

module ProblemSet01.MakePS01 (mkps01) where

import Text.LaTeX
import Text.LaTeX.Base.Commands
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.Geometry
import Text.LaTeX.Packages.Inputenc   -- usepackage [utf8] inputenc
import Text.LaTeX.Base.Pretty         -- prettyLaTeX
import Text.LaTeX.Packages.Trees.Qtree
import Text.LaTeX.Packages.AMSSymb
import Text.LaTeX.Base.Math


import Translations.RandomSentences

-- |GENERAL DOCUMENT BUILDING FUNCTIONS

-- |function to render questions and answers to .tex file
mkps01 :: IO ()
mkps01 = do
         (q1q,q1a) <- getq1 
         (q2q,q2a) <- getq2
         renderFile "ps01q.tex" (ps01q (q1q,q2q)) -- render questions to tex
         renderFile "ps01a.tex" (ps01a (q1q,q1a) (q2q,q2a)) -- render answers to tex

-- |here we get the random prop(s), make the tree, return the LaTeX versions

getq1 :: IO (LaTeX,LaTeX)
getq1 = pltranslation

getq2 :: IO (LaTeX,LaTeX)
getq2 = pltranslation 


-- |document preamble

-- |preamble for questions
ps01pq :: LaTeX 
ps01pq = docSettings <> title "Problem Set 01: PL Translations (Questions)" <> author "" <> date ""

-- |preamble for answers
ps01pa :: LaTeX 
ps01pa = docSettings <> title "Problem Set 01: PL Translations (Answers)" <> author "" <> date ""

-- |shared document settings
docSettings :: LaTeX
docSettings = documentclass [] article 
            <> usepackage [] amssymb 
            <> usepackage [utf8] inputenc 
            <> usepackage [] qtree 
            <> importGeometry [GWidth (Cm 18)]

-- |final latex document to render

-- |only questions
ps01q :: (LaTeX,LaTeX) -> LaTeX
ps01q x = ps01pq <> document (maketitle <> questions x)

-- |with answers
ps01a :: (LaTeX,LaTeX) -> (LaTeX,LaTeX) -> LaTeX
ps01a x y = ps01pa <> document (maketitle <> answers x y)

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




