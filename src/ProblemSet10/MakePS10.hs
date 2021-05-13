{-# LANGUAGE OverloadedStrings #-}

module ProblemSet10.MakePS10 (mkps10) where


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
import Printing.LaTeXGPLIModel (printmodel, printmodellns, printmodels)

import Random.GPLIprop (gplisat,gplival,prepforvalidity)
import Trees.GPLItrees (mktree, getmodel, getmodels)

-- |GENERAL DOCUMENT BUILDING FUNCTIONS

-- |function to render questions and answers to .tex file
mkps10 :: IO ()
mkps10 = do
         (q1q,q1a) <- getq1 
         (q2q,q2a) <- getq2
         renderFile "ps10q.tex" (ps10q (q1q,q2q)) -- render questions to tex
         renderFile "ps10a.tex" (ps10a (q1q,q1a) (q2q,q2a)) -- render answers to tex

-- |here we get the random prop(s), make the tree, return the LaTeX versions

getq1 :: IO (LaTeX,LaTeX)
getq1 = do
        p <- gplisat
        let t = mktree p
        return (printprops p, printtree t <> quote (printmodels $ getmodels t) )

getq2 :: IO (LaTeX,LaTeX)
getq2 = do
        p <- gplival
        let t = mktree (prepforvalidity p)
        return (printarg p, printtree t)

-- |document preamble

-- |preamble for questions
ps10pq :: LaTeX 
ps10pq = docSettings <> title "Problem Set 10: GPLI Trees (Questions)" <> author "" <> date ""

-- |preamble for answers
ps10pa :: LaTeX 
ps10pa = docSettings <> title "Problem Set 10: GPLI Trees (Answers)" <> author "" <> date ""

-- |shared document settings
docSettings :: LaTeX
docSettings = documentclass [] article 
            <> usepackage [] amssymb 
            <> usepackage [utf8] inputenc 
            <> usepackage [] qtree 
            <> importGeometry [GWidth (Cm 16)]

-- |final latex document to render

-- |only questions
ps10q :: (LaTeX,LaTeX) -> LaTeX
ps10q x = ps10pq <> document (maketitle <> questions x)

-- |with answers
ps10a :: (LaTeX,LaTeX) -> (LaTeX,LaTeX) -> LaTeX
ps10a x y = ps10pa <> document (maketitle <> answers x y)

-- |DOCUMENT BODY

-- |shared

-- | the text of q1
q1text :: LaTeX
q1text = item Nothing <> "Use a tree to test whether the following propositions are jointly satisfiable. If they are, then read a model off the tree."

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

-- |question 2 question and answer template
q2atemp :: LaTeX -> LaTeX -> LaTeX
q2atemp q a = item Nothing <> q <> item Nothing <> "Answer:" <> quote a

-- |only questions template -- expects each question in order
questions :: (LaTeX,LaTeX) -> LaTeX
questions (x,y) = enumerate (q1text <> enumerate (q1qtemp x) <> q2text <> enumerate (q2qtemp y))

-- |questions and answers template -- expects question and answer pairs
answers :: (LaTeX,LaTeX) -> (LaTeX,LaTeX) -> LaTeX
answers (xq,xa) (yq,ya) = enumerate (q1text <> enumerate (q1atemp xq xa) <> q2text <> enumerate (q2atemp yq ya))




