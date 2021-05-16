{-# LANGUAGE OverloadedStrings #-}

module MakePS.MakePS07 (mkps07g) where

import Text.LaTeX
    ( IsString(fromString),
      LaTeX,
      article,
      author,
      date,
      document,
      documentclass,
      enumerate,
      item,
      maketitle,
      quote,
      title,
      usepackage,
      renderFile,
      Measure(Cm) )
import Text.LaTeX.Base.Commands
    ( article,
      author,
      date,
      document,
      documentclass,
      enumerate,
      item,
      maketitle,
      quote,
      title,
      usepackage )
import Text.LaTeX.Base.Syntax ( LaTeX, Measure(Cm) )
import Text.LaTeX.Packages.Geometry
    ( importGeometry, GeometryOption(GWidth) )
import Text.LaTeX.Packages.Inputenc ( inputenc, utf8 )   
import Text.LaTeX.Packages.Trees.Qtree ( qtree )
import Text.LaTeX.Packages.AMSSymb ( amssymb )

import Printing.LaTeXGPLIProps (printprops,printarg)
import Printing.LaTeXGPLITrees (printtree)
import Printing.LaTeXGPLIModel (printmodels)

import System.Random ( RandomGen(split) )

import Random.GPLIprop (mplequivg, mplsatg, prepforequiv, gpltautstats, gpltaut,gplsat,gplisat,gplival,prepforvalidity,prepfortaut)
import Trees.GPLItrees ( mktree, getmodels )

-- |GENERAL DOCUMENT BUILDING FUNCTIONS

-- |function to render questions and answers to .tex file
mkps07g :: RandomGen g => g -> Int -> IO ()
mkps07g g n = do
         let (q1q,q1a) =  getq1g g1
         let (q2q,q2a) = getq2g g2
         renderFile ("ps07" ++ "-" ++ (show n) ++ "q.tex") (ps07q (q1q,q2q) n) -- render questions to tex
         renderFile ("ps07" ++ "-" ++ (show n) ++ "a.tex") (ps07a (q1q,q1a) (q2q,q2a) n) -- render answers to tex
         where (g1,g2) = split g

-- |here we get the random prop(s), make the tree, return the LaTeX versions

getq1g :: RandomGen g => g ->  (LaTeX,LaTeX)
getq1g g  =  let p = mplequivg g in
             let t = mktree (prepforequiv p) in
             (printprops p, printtree t)

getq2g :: RandomGen g => g ->  (LaTeX,LaTeX)
getq2g g = let  p = mplsatg g in
           let t = mktree p in
           (printprops p, printtree t <> quote (printmodels $ getmodels t))

-- |document preamble

-- |preamble for questions
ps07pq :: Int -> LaTeX
ps07pq n = docSettings <> title "Problem Set 07: MPL Trees (Questions)" <> author "" <> date (fromString $ "#" ++ show n)

-- |preamble for answers
ps07pa :: Int -> LaTeX
ps07pa n = docSettings <> title "Problem Set 07: MPL Trees (Answers)" <> author "" <> date (fromString $ "#" ++ show n)

-- |shared document settings
docSettings :: LaTeX
docSettings = documentclass [] article
            <> usepackage [] amssymb
            <> usepackage [utf8] inputenc
            <> usepackage [] qtree
            <> importGeometry [GWidth (Cm 18)]

-- |final latex document to render

-- |only questions
ps07q :: (LaTeX,LaTeX) -> Int ->  LaTeX
ps07q x n = ps07pq n <> document (maketitle <> questions x)

-- |with answers
ps07a :: (LaTeX,LaTeX) -> (LaTeX,LaTeX) -> Int -> LaTeX
ps07a x y n = ps07pa n <> document (maketitle <> answers x y)

-- |DOCUMENT BODY

-- |shared

-- | the text of q1
q1text :: LaTeX
q1text = item Nothing <> "Use a tree to test whether the following are equivalent. If they are not, then read a countermodel off the tree."

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




