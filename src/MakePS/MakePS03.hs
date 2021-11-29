{-# LANGUAGE OverloadedStrings #-}

-- normal forms

module MakePS.MakePS03 (mkps03g, mkps03html) where


import Text.LaTeX
import Text.LaTeX.Base.Commands
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.Geometry
import Text.LaTeX.Packages.Inputenc   -- usepackage [utf8] inputenc
import Text.LaTeX.Base.Pretty         -- prettyLaTeX
import Text.LaTeX.Packages.Trees.Qtree
import Text.LaTeX.Packages.AMSSymb
import Text.LaTeX.Base.Math

import qualified Printing.HTMLPLTrees as PHT
import qualified Printing.HTMLTables as PH
import qualified Printing.UnicodePLProps as UP
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H5
import Printing.LaTeXPLProps
import Printing.LaTeXTables
import Tables.Tables

import Trees.PLtreesNew (mktree)
import Printing.LaTeXPLTrees (printtree) 

import NormalForms.PLnormalforms

import Random.PLprops (justanrprop, prepforequiv' )

import Settings.PLSettings

import System.Random 

mkps03html :: IO H.Html
mkps03html = do
       g <- newStdGen    -- get random generator
       let (num,_) = next g  -- use it to get a random number
       let seed = mkStdGen num
       let (g1,g2) = split seed   
       let (g3,g4) = split g1     
       let (q1q,q1a) = getq1g g1
       let (q2q,q2a) = getq2g g2
       let (q3q,q3a) = getq3g g3
       let questionstring = prettyLaTeX (ps03q (q1q,q2q,q3q) num)
       let answerstring = prettyLaTeX (ps03a (q1q,q1a) (q2q,q2a) (q3q,q3a) num)
       (q1qh,q1ah) <- getq1gh g1
       (q2qh,q2ah) <- getq2gh g2
       (q3qh,q3ah) <- getq3gh g3
       return (htmltemplate $ QandASet q1qh q2qh q3qh q1ah q2ah q3ah questionstring answerstring)

data QandASet = QandASet {htmlQ1 :: H.Html
                         ,htmlQ2 :: H.Html
                         ,htmlQ3
                         ,htmlQA1 :: H.Html
                         ,htmlQA2 :: H.Html
                         ,htmlQA3 :: H.Html
                         ,latexQS :: String
                         ,latexQAS :: String}

htmltemplate :: QandASet -> H.Html
htmltemplate qa = do
       H5.h1 $ H.toHtml ("Problem Set 3: Normal Forms" :: String)
       H5.h2 $ H.toHtml ("Just the Questions" :: String)
       H5.p $ H.toHtml ("Q1. Convert the following proposition into negation normal form. Then show that the proposition in negation normal form is equivalent to the original proposition (using either a truth table, a truth tree, or both)." :: Text)
       H5.p $ htmlQ1 qa
       H5.p $ H.toHtml ("Q2. Convert the following proposition into conjunctive normal form. Then show that the proposition in conjunctive normal form is equivalent to the original proposition (using either a truth table, a truth tree, or both)." :: Text)
       H5.p $ htmlQ2 qa
       H5.p $ H.toHtml ("Q3. Convert the following proposition into disjunctive normal form. Then show that the proposition in disjunctive normal form is equivalent to the original proposition (using either a truth table, a truth tree, or both)." :: Text)
       H5.p $ htmlQ3 qa
       H5.h2 $ H.toHtml ("Questions and Answers" :: String)
       H5.p $ H.toHtml ("Q1. Convert the following proposition into negation normal form. Then show that the proposition in negation normal form is equivalent to the original proposition (using either a truth table, a truth tree, or both)." :: Text)
       H5.p $ htmlQ1 qa
       H5.p $ htmlQA1 qa
       H5.p $ H.toHtml ("Q2. Convert the following proposition into conjunctive normal form. Then show that the proposition in conjunctive normal form is equivalent to the original proposition (using either a truth table, a truth tree, or both)." :: Text)
       H5.p $ htmlQ2 qa
       H5.p $ htmlQA2 qa
       H5.p $ H.toHtml ("Q3. Convert the following proposition into disjunctive normal form. Then show that the proposition in disjunctive normal form is equivalent to the original proposition (using either a truth table, a truth tree, or both)." :: Text)
       H5.p $ htmlQ3 qa
       H5.p $ htmlQA3 qa
       H5.h2 $ H.toHtml ("Just the Questions (LaTeX)" :: String)
       H5.p $ H.toHtml (latexQS qa)
       H5.h2 $ H.toHtml ("Questions and Answers (LaTeX)" :: String)
       H5.p $ H.toHtml (latexQAS qa)

-- |tree building


-- |GENERAL DOCUMENT BUILDING FUNCTIONS

-- |function to render questions and answers to .tex file
mkps03g :: RandomGen g => g -> Int -> IO ()
mkps03g g n = do
              let (q1q,q1a) = getq1g g1
              let (q2q,q2a) = getq2g g2
              let (q3q,q3a) = getq3g g3
              renderFile ("ps03" ++ "-" ++ show n ++ "q.tex") (ps03q (q1q,q2q,q3q) n) -- render questions to tex
              renderFile ("ps03" ++ "-" ++ show n ++ "a.tex") (ps03a (q1q,q1a) (q2q,q2a) (q3q,q3a) n) -- render answers to tex
                  where (g1,g2) = split g
                        (g3,g4) = split g2
-- |here we get the random prop(s), make the tree, return the LaTeX versions

getq1g :: RandomGen g => g -> (LaTeX,LaTeX)
getq1g g = let p = justanrprop g dSettings in
           let n = nnf p in
           let t = makeRawTable [p,n] in
           let tr = mktree (prepforequiv' p n) in
           (printprop p, printprop n <> makeTable t <> printtree tr)

getq2g :: RandomGen g => g -> (LaTeX,LaTeX)
getq2g g = let p = justanrprop g dSettings in
           let c = cnf p in
           let t = makeRawTable [p,c] in
           let tr = mktree (prepforequiv' p c) in
           (printprop p, printprop c <> makeTable t <> printtree tr )

getq3g :: RandomGen g => g -> (LaTeX,LaTeX)
getq3g g = let p = justanrprop g dSettings in
           let d = dnf p in
           let t = makeRawTable [p,d] in
           let tr = mktree (prepforequiv' p d ) in
           (printprop p, printprop d <> makeTable t <> printtree tr)

-- html versions of the above

getq1gh :: RandomGen g => g -> IO (H.Html,H.Html)
getq1gh g = do
           let p = justanrprop g dSettings
           let n = nnf p
           let t = makeRawTable [p,n]
           let tr = mktree (prepforequiv' p n)
           tree <- PHT.printtree tr
           return (H5.p $ H.toHtml $ UP.printprops [p], H5.p (H.toHtml ("Negation normal form: " :: String)) <> H5.p (H.toHtml (UP.printprops [n])) <> PH.makeTable t <> H5.p (H.toHtml ("Tree: " :: String)) <> tree)

getq2gh :: RandomGen g => g -> IO (H.Html,H.Html)
getq2gh g = do
           let p = justanrprop g dSettings
           let c = cnf p
           let t = makeRawTable [p,c]
           let tr = mktree (prepforequiv' p c)
           tree <- PHT.printtree tr
           return (H5.p $ H.toHtml $  UP.printprops [p], H5.p (H.toHtml ("Conjunctive normal form: " :: String)) <> H5.p (H.toHtml ( UP.printprops [c])) <> PH.makeTable t <> H5.p (H.toHtml ("Tree: " :: String)) <> tree)

getq3gh :: RandomGen g => g -> IO (H.Html,H.Html)
getq3gh g = do
           let p = justanrprop g dSettings
           let d = dnf p
           let t = makeRawTable [p,d]
           let tr = mktree (prepforequiv' p d)
           tree <- PHT.printtree tr
           return (H5.p $ H.toHtml $  UP.printprops [p], H5.p (H.toHtml ("Disjunctive normal form: " :: String)) <> H5.p (H.toHtml (UP.printprops [d])) <> PH.makeTable t <> H5.p (H.toHtml ("Tree: " :: String)) <> tree)

-- |document preamble

-- |preamble for questions
ps03pq :: Int -> LaTeX 
ps03pq n = docSettings <> title "Problem Set 02: PL Tables (Questions)" <> author "" <> date (fromString $ "#" ++ show n)

-- |preamble for answers
ps03pa :: Int -> LaTeX 
ps03pa n = docSettings <> title "Problem Set 02: PL Tables (Answers)" <> author "" <> date (fromString $ "#" ++ show n)

-- |shared document settings
docSettings :: LaTeX
docSettings = documentclass [] article 
            <> usepackage [] amssymb 
            <> usepackage [utf8] inputenc 
            <> usepackage [] qtree 
            <> importGeometry [GWidth (Cm 18)]

-- |final latex document to render

-- |only questions
ps03q :: (LaTeX,LaTeX,LaTeX) -> Int -> LaTeX
ps03q x n = ps03pq n <> document (maketitle <> questions x)

-- |with answers
ps03a :: (LaTeX,LaTeX) -> (LaTeX,LaTeX) -> (LaTeX,LaTeX) -> Int -> LaTeX
ps03a x y z n = ps03pa n <> document (maketitle <> answers x y z)

-- |DOCUMENT BODY

-- |shared

-- | the text of q1
q1text :: LaTeX
q1text = item Nothing <> "Convert the following proposition into negation normal form. Then show that the proposition in negation normal form is equivalent to the original proposition (using either a truth table, a truth tree, or both)."

-- | the text of q2
q2text :: LaTeX
q2text = item Nothing <> "Convert the following proposition into conjunctive normal form. Then show that the proposition in conjunctive normal form is equivalent to the original proposition (using either a truth table, a truth tree, or both)."

-- | the text of q3
q3text :: LaTeX
q3text = item Nothing <> "Convert the following proposition into disjunctive normal form. Then show that the proposition in disjunctive normal form is equivalent to the original proposition (using either a truth table, a truth tree, or both)."


-- |template for just the questions

-- |question 1 question template, expects the question in LaTeX
q1qtemp :: LaTeX -> LaTeX
q1qtemp q = item Nothing <> q

-- |question 2 question template
q2qtemp :: LaTeX -> LaTeX
q2qtemp q = item Nothing <> q

-- |question 3 question template
q3qtemp :: LaTeX -> LaTeX
q3qtemp q = item Nothing <> q

-- |template for questions and answers

-- |question 1 question and answer template
q1atemp :: LaTeX -> LaTeX -> LaTeX
q1atemp q a = item Nothing <> q <> item Nothing <> "Answer:" <> quote a

-- |question 2 question and answer template
q2atemp :: LaTeX -> LaTeX -> LaTeX
q2atemp q a = item Nothing <> q <> item Nothing <> "Answer:" <> quote a

-- |question 3 question and answer template
q3atemp :: LaTeX -> LaTeX -> LaTeX
q3atemp q a = item Nothing <> q <> item Nothing <> "Answer:" <> quote a

-- |only questions template -- expects each question in order
questions :: (LaTeX,LaTeX,LaTeX) -> LaTeX
questions (x,y,z) = enumerate (q1text <> enumerate (q1qtemp x) <> q2text <> enumerate (q2qtemp y) <> q3text <> enumerate (q3qtemp z))

-- |questions and answers template -- expects question and answer pairs
answers :: (LaTeX,LaTeX) -> (LaTeX,LaTeX) -> (LaTeX,LaTeX) -> LaTeX
answers (xq,xa) (yq,ya) (zq,za) = enumerate (q1text <> enumerate (q1atemp xq xa) <> q2text <> enumerate (q2atemp yq ya) <> q3text <> enumerate (q3atemp zq za))




