{-# LANGUAGE OverloadedStrings #-}

module MakePS.MakePS04 (mkps04g) where


import Text.LaTeX
import Text.LaTeX.Base.Commands
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.Geometry
import Text.LaTeX.Packages.Inputenc   -- usepackage [utf8] inputenc
import Text.LaTeX.Base.Pretty         -- prettyLaTeX
import Text.LaTeX.Packages.Trees.Qtree
import Text.LaTeX.Packages.AMSSymb
import Text.LaTeX.Base.Math

import Printing.LaTeXPLProps (printprops,printarg) 
import Printing.LaTeXPLTrees (printtree) 

import System.Random ( RandomGen(split) )

import Random.PLprops (plcontrariesg, prepfc, plvalidg, prepforvalidity)
import Trees.PLtrees (mktree)

-- |GENERAL DOCUMENT BUILDING FUNCTIONS

-- |function to render questions and answers to .tex file
mkps04g :: RandomGen g => g -> Int -> IO ()
mkps04g g n = do
              let (q1q,q1a1,q1a2) = getq1g g1
              let (q2q,q2a) = getq2g g2
              renderFile ("ps04" ++ "-" ++ (show n) ++ "q.tex") (ps04q (q1q,q2q) n) -- render questions to tex
              renderFile ("ps04" ++ "-" ++ (show n) ++ "a.tex") (ps04a (q1q,q1a1,q1a2) (q2q,q2a) n) -- render answers to tex
                  where (g1,g2) = split g
-- |here we get the random prop(s), make the tree, return the LaTeX versions

getq1g :: RandomGen g => g -> (LaTeX,LaTeX,LaTeX)
getq1g g = let p = plcontrariesg g in
          let t1 = mktree (prepfc p) in
          let t2 = mktree p in
          (printprops p, printtree t2, printtree t1)

getq2g :: RandomGen g => g -> (LaTeX,LaTeX)
getq2g g = let p = plvalidg g in
           let t = mktree (prepforvalidity p) in
           (printprops p, printtree t)

-- |document preamble

-- |preamble for questions
ps04pq :: Int -> LaTeX 
ps04pq n = docSettings <> title "Problem Set 04: PL Trees (Questions)" <> author "" <> date (fromString $ "#" ++ show n)

-- |preamble for answers
ps04pa :: Int -> LaTeX 
ps04pa n = docSettings <> title "Problem Set 04: PL Trees (Answers)" <> author "" <> date (fromString $ "#" ++ show n)

-- |shared document settings
docSettings :: LaTeX
docSettings = documentclass [] article 
            <> usepackage [] amssymb 
            <> usepackage [utf8] inputenc 
            <> usepackage [] qtree 
            <> importGeometry [GWidth (Cm 18)]

-- |final latex document to render

-- |only questions
ps04q :: (LaTeX,LaTeX) -> Int -> LaTeX
ps04q x n = ps04pq n <> document (maketitle <> questions x)

-- |with answers
ps04a :: (LaTeX,LaTeX,LaTeX) -> (LaTeX,LaTeX) -> Int ->  LaTeX
ps04a x y n = ps04pa n <> document (maketitle <> answers' x y)

-- |DOCUMENT BODY

-- |shared

-- | the text of q1
q1text :: LaTeX
q1text = item Nothing <> "Use a tree to test whether the following are contraries. If they are not, then read a countermodel off the tree."

-- | the text of q2
q2text :: LaTeX
q2text = item Nothing <> "Use a tree to test whether the following argument is valid. If it is not, then read a countermodel off the tree."

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


-- |question 1 question and answer template
q1atemp' :: LaTeX -> LaTeX -> LaTeX -> LaTeX
q1atemp' q a1 a2 = item Nothing <> q <> item Nothing <> "Answer:" <> quote a1 <> quote a2


-- |question 2 question and answer template
q2atemp :: LaTeX -> LaTeX -> LaTeX
q2atemp q a = item Nothing <> q <> item Nothing <> "Answer:" <> quote a



-- |only questions template -- expects each question in order
questions :: (LaTeX,LaTeX) -> LaTeX
questions (x,y) = enumerate (q1text <> enumerate (q1qtemp x) <> q2text <> enumerate (q2qtemp y))

-- |questions and answers template -- expects question and answer pairs
answers :: (LaTeX,LaTeX) -> (LaTeX,LaTeX) -> LaTeX
answers (xq,xa) (yq,ya) = enumerate (q1text <> enumerate (q1atemp xq xa) <> q2text <> enumerate (q2atemp yq ya))

answers' :: (LaTeX,LaTeX,LaTeX) -> (LaTeX,LaTeX) -> LaTeX
answers' (xq,xa1,xa2) (yq,ya) = enumerate (q1text <> enumerate (q1atemp' xq xa1 xa2) <> q2text <> enumerate (q2atemp yq ya))





