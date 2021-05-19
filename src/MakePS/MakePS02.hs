{-# LANGUAGE OverloadedStrings #-}

module MakePS.MakePS02 (mkps02g) where


import Text.LaTeX
import Text.LaTeX.Base.Commands
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.Geometry
import Text.LaTeX.Packages.Inputenc   -- usepackage [utf8] inputenc
import Text.LaTeX.Base.Pretty         -- prettyLaTeX
import Text.LaTeX.Packages.Trees.Qtree
import Text.LaTeX.Packages.AMSSymb
import Text.LaTeX.Base.Math

import Printing.LaTeXPLProps
import Printing.LaTeXTables
import Tables.Tables

import Random.PLprops ( plvalidg, plequivsg )

import Settings.PLSettings

import System.Random ( RandomGen(split) )

-- |tree building


-- |GENERAL DOCUMENT BUILDING FUNCTIONS

-- |function to render questions and answers to .tex file
mkps02g :: RandomGen g => g -> Int -> IO ()
mkps02g g n = do
              let (q1q,q1a) = getq1g g1
              let (q2q,q2a) = getq2g g2
              renderFile ("ps02" ++ "-" ++ (show n) ++ "q.tex") (ps02q (q1q,q2q) n) -- render questions to tex
              renderFile ("ps02" ++ "-" ++ (show n) ++ "a.tex") (ps02a (q1q,q1a) (q2q,q2a) n) -- render answers to tex
                  where (g1,g2) = split g
-- |here we get the random prop(s), make the tree, return the LaTeX versions

getq1g :: RandomGen g => g -> (LaTeX,LaTeX)
getq1g g = let p = plequivsg g settingPS02a in
           let t = makeRawTable p in
           (printprops p, makeTable t)

getq2g :: RandomGen g => g -> (LaTeX,LaTeX)
getq2g g = let p = plvalidg g settingPS02b in
           let t = makeRawTable p in
           (printarg p, makeTable t)

-- |document preamble

-- |preamble for questions
ps02pq :: Int -> LaTeX 
ps02pq n = docSettings <> title "Problem Set 02: PL Tables (Questions)" <> author "" <> date (fromString $ "#" ++ show n)

-- |preamble for answers
ps02pa :: Int -> LaTeX 
ps02pa n = docSettings <> title "Problem Set 02: PL Tables (Answers)" <> author "" <> date (fromString $ "#" ++ show n)

-- |shared document settings
docSettings :: LaTeX
docSettings = documentclass [] article 
            <> usepackage [] amssymb 
            <> usepackage [utf8] inputenc 
            <> usepackage [] qtree 
            <> importGeometry [GWidth (Cm 18)]

-- |final latex document to render

-- |only questions
ps02q :: (LaTeX,LaTeX) -> Int -> LaTeX
ps02q x n = ps02pq n <> document (maketitle <> questions x)

-- |with answers
ps02a :: (LaTeX,LaTeX) -> (LaTeX,LaTeX) -> Int -> LaTeX
ps02a x y n = ps02pa n <> document (maketitle <> answers x y)

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




