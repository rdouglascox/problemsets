{-# LANGUAGE OverloadedStrings #-}

-- | gpli trees

module MakePS.MakePS10 (mkps10g, mkps10string, mkps10html) where

import Text.LaTeX
import Text.LaTeX.Base.Commands
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.Geometry
import Text.LaTeX.Packages.Inputenc   -- usepackage [utf8] inputenc
import Text.LaTeX.Base.Pretty         -- prettyLaTeX
import Text.LaTeX.Packages.Trees.Qtree
import Text.LaTeX.Packages.AMSSymb
import Text.LaTeX.Base.Math

import System.Random

import Printing.LaTeXGPLIProps (printprops,printarg) 
import Printing.LaTeXGPLITrees (printtree) 
import Printing.LaTeXGPLIModel (printmodel, printmodellns, printmodels)

import Random.GPLIprop (gplisatg,gplivalg,prepforvalidity)
import Trees.GPLItrees (mktree, getmodel, getmodels)

import Settings.GPLISettings ( settingPS10b, settingPS10a )

import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H5
import qualified Printing.UnicodeGPLIProps as U
import qualified Printing.RenderSVGGPLI as R
import qualified Printing.TextGPLIModel as T
import qualified Printing.HTMLGPLITrees as PHT
import qualified Printing.HTMLGPLIModel as HM

-- |GENERAL DOCUMENT BUILDING FUNCTIONS

mkps10html :: IO H.Html
mkps10html = do
       g <- newStdGen    -- get random generator
       let (num,_) = next g  -- use it to get a random number
       let seed = mkStdGen num
       let (g1,g2) = split seed        
       let (q1q,q1a) = getq1g g1
       let (q2q,q2a) = getq2g g2
       let questionstring = prettyLaTeX (ps10q (q1q,q2q) num)
       let answerstring = prettyLaTeX (ps10a (q1q,q1a) (q2q,q2a) num)
       (q1qh,q1ah) <- getq1gh g1
       (q2qh,q2ah) <- getq2gh g2
       return (htmltemplate $ QandASet q1qh q2qh q1ah q2ah questionstring answerstring)

data QandASet = QandASet {htmlQ1 :: H.Html
                         ,htmlQ2 :: H.Html
                         ,htmlQA1 :: H.Html
                         ,htmlQA2 :: H.Html
                         ,latexQS :: String
                         ,latexQAS :: String}

htmltemplate :: QandASet -> H.Html
htmltemplate qa = do
       H5.h1 $ H.toHtml ("Problem Set 10: GPLI Truth Trees" :: String)
       H5.h2 $ H.toHtml ("Just the Questions" :: String)
       H5.p $ H.toHtml ("Q1. Use a tree to test whether the following propositions are jointly satisfiable. If they are, then read a model off the tree." :: String)
       H5.p $ htmlQ1 qa
       H5.p $ H.toHtml ("Q2. Use a tree to test whether the following argument is valid. If it is not, then read a countermodel off the tree." :: String)
       H5.p $ htmlQ2 qa
       H5.h2 $ H.toHtml ("Questions and Answers" :: String)
       H5.p $ H.toHtml ("Q1. Use a tree to test whether the following propositions are jointly satisfiable. If they are, then read a model off the tree." :: String)
       H5.p $ htmlQ1 qa
       H5.p $ H.toHtml ("Tree:" :: String)
       H5.p $ htmlQA1 qa
       H5.p $ H.toHtml ("Q2. Use a tree to test whether the following argument is valid. If it is not, then read a countermodel off the tree." :: String)
       H5.p $ htmlQ2 qa
       H5.p $ H.toHtml ("Tree:" :: String)
       H5.p $ htmlQA2 qa
       H5.h2 $ H.toHtml ("Just the Questions (LaTeX)" :: String)
       H5.p $ H.toHtml (latexQS qa)
       H5.h2 $ H.toHtml ("Questions and Answers (LaTeX)" :: String)
       H5.p $ H.toHtml (latexQAS qa)

-- | just give me a string man!
mkps10string :: IO (String, String)
mkps10string = do
       g <- newStdGen    -- get random generator
       let (num,_) = next g  -- use it to get a random number
       let seed = mkStdGen num
       let (g1,g2) = split seed        
       let (q1q,q1a) = getq1g g1
       let (q2q,q2a) = getq2g g2
       let questionstring = prettyLaTeX (ps10q (q1q,q2q) num)
       let answerstring = prettyLaTeX (ps10a (q1q,q1a) (q2q,q2a) num)
       return (questionstring,answerstring)


-- |function to render questions and answers to .tex file
mkps10g :: RandomGen g => g -> Int -> IO ()
mkps10g g n = do
         let (q1q,q1a) = getq1g g1
         let (q2q,q2a) = getq2g g2
         renderFile ("ps10" ++ "-" ++ show n ++ "q.tex") (ps10q (q1q,q2q) n) -- render questions to tex
         renderFile ("ps10" ++ "-" ++ show n ++ "a.tex") (ps10a (q1q,q1a) (q2q,q2a) n) -- render answers to tex
        where (g1,g2) = split g

-- |here we get the random prop(s), make the tree, return the LaTeX versions

getq1g :: RandomGen g => g ->  (LaTeX,LaTeX)
getq1g g = let p = gplisatg g settingPS10a in
           let t = mktree p in
           (printprops p, printtree t <> quote (printmodels $ getmodels t) )

getq2g :: RandomGen g => g ->  (LaTeX,LaTeX)
getq2g g = let p = gplivalg g settingPS10b in
           let t = mktree (prepforvalidity p) in
           (printarg p, printtree t)

getq1gh :: RandomGen g => g -> IO (H.Html,H.Html)
getq1gh g  = do
             let p = gplisatg g settingPS10a
             let t = mktree p
             pt <- PHT.printtree t
             return (H.toHtml $ U.printprops p, pt)

getq2gh :: RandomGen g => g -> IO (H.Html,H.Html)
getq2gh g = do
            let p = gplivalg g settingPS10b
            let t = mktree p
            pt <- PHT.printtree t
            return (H.toHtml $ U.printprops p, pt <> H5.br <> H5.p (HM.printmodels (getmodels t)))

-- |document preamble

-- |preamble for questions
ps10pq :: Int -> LaTeX 
ps10pq n = docSettings <> title "Problem Set 10: GPLI Trees (Questions)" <> author "" <> date (fromString $ "#" ++ show n)

-- |preamble for answers
ps10pa :: Int -> LaTeX 
ps10pa n = docSettings <> title "Problem Set 10: GPLI Trees (Answers)" <> author "" <> date (fromString $ "#" ++ show n)

-- |shared document settings
docSettings :: LaTeX
docSettings = documentclass [] article 
            <> usepackage [] amssymb 
            <> usepackage [utf8] inputenc 
            <> usepackage [] qtree 
            <> importGeometry [GWidth (Cm 16)]

-- |final latex document to render

-- |only questions
ps10q :: (LaTeX,LaTeX) -> Int -> LaTeX
ps10q x n = ps10pq n <> document (maketitle <> questions x)

-- |with answers
ps10a :: (LaTeX,LaTeX) -> (LaTeX,LaTeX) -> Int ->  LaTeX
ps10a x y n = ps10pa n <> document (maketitle <> answers x y)

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




