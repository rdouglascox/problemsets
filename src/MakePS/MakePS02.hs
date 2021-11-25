{-# LANGUAGE OverloadedStrings #-}

-- pl truth tables

module MakePS.MakePS02 (mkps02g, mkps02string, mkps02html) where


import Text.LaTeX
import Text.LaTeX.Base.Commands
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.Geometry
import Text.LaTeX.Packages.Inputenc   -- usepackage [utf8] inputenc
import Text.LaTeX.Base.Pretty         -- prettyLaTeX
import Text.LaTeX.Packages.Trees.Qtree
import Text.LaTeX.Packages.AMSSymb
import Text.LaTeX.Base.Math

import qualified Printing.HTMLTables as PH
import qualified Printing.UnicodePLProps as UP
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H5
import Printing.LaTeXPLProps
import Printing.LaTeXTables
import Tables.Tables


import Random.PLprops ( plvalidg, plequivsg )

import Settings.PLSettings

import System.Random 

mkps02html :: IO H.Html
mkps02html = do
       g <- newStdGen    -- get random generator
       let (num,_) = next g  -- use it to get a random number
       let seed = mkStdGen num
       let (g1,g2) = split seed        
       let (q1q,q1a) = getq1g g1
       let (q2q,q2a) = getq2g g2
       let questionstring = prettyLaTeX (ps02q (q1q,q2q) num)
       let answerstring = prettyLaTeX (ps02a (q1q,q1a) (q2q,q2a) num)
       let (q1qh,q1ah) = getq1gh g1
       let (q2qh,q2ah) = getq2gh g2
       return (htmltemplate $ QandASet q1qh q2qh q1ah q2ah questionstring answerstring)

data QandASet = QandASet {htmlQ1 :: H.Html
                         ,htmlQ2 :: H.Html
                         ,htmlQA1 :: H.Html
                         ,htmlQA2 :: H.Html
                         ,latexQS :: String
                         ,latexQAS :: String}

htmltemplate :: QandASet -> H.Html
htmltemplate qa = do
       H5.h2 $ H.toHtml ("Just the Questions" :: String)
       H5.p $ H.toHtml ("Q1. Use a truth table to test whether the following propositions are equivalient." :: Text)
       H5.p $ htmlQ1 qa
       H5.p $ H.toHtml ("Q2. Use a truth table to test whether the following argument is valid." :: Text)
       H5.p $ htmlQ2 qa
       H5.h2 $ H.toHtml ("Questions and Answers" :: String)
       H5.p $ H.toHtml ("Q1. Use a truth table to test whether the following propositions are equivalient." :: Text)
       H5.p $ htmlQ1 qa
       H5.p $ htmlQA1 qa
       H5.p $ H.toHtml ("Q2. Use a truth table to test whether the following argument is valid." :: Text)
       H5.p $ htmlQ2 qa
       H5.p $ htmlQA2 qa
       H5.h2 $ H.toHtml ("Just the Questions (LaTeX)" :: String)
       H5.p $ H.toHtml (latexQS qa)
       H5.h2 $ H.toHtml ("Questions and Answers (LaTeX)" :: String)
       H5.p $ H.toHtml (latexQAS qa)

-- |tree building

-- | just give me a string man!
mkps02string :: IO (String, String)
mkps02string = do
       g <- newStdGen    -- get random generator
       let (num,_) = next g  -- use it to get a random number
       let seed = mkStdGen num
       let (g1,g2) = split seed        
       let (q1q,q1a) = getq1g g1
       let (q2q,q2a) = getq2g g2
       let questionstring = prettyLaTeX (ps02q (q1q,q2q) num)
       let answerstring = prettyLaTeX (ps02a (q1q,q1a) (q2q,q2a) num)
       return (questionstring,answerstring)

-- |GENERAL DOCUMENT BUILDING FUNCTIONS

-- |function to render questions and answers to .tex file
mkps02g :: RandomGen g => g -> Int -> IO ()
mkps02g g n = do
              let (q1q,q1a) = getq1g g1
              let (q2q,q2a) = getq2g g2
              renderFile ("ps02" ++ "-" ++ show n ++ "q.tex") (ps02q (q1q,q2q) n) -- render questions to tex
              renderFile ("ps02" ++ "-" ++ show n ++ "a.tex") (ps02a (q1q,q1a) (q2q,q2a) n) -- render answers to tex
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

-- html versions of the above

getq1gh :: RandomGen g => g -> (H.Html,H.Html)
getq1gh g = let p = plequivsg g settingPS02a in
           let t = makeRawTable p in
           (H.toHtml $ UP.printprops p, PH.makeTable t)

getq2gh :: RandomGen g => g -> (H.Html,H.Html)
getq2gh g = let p = plvalidg g settingPS02b in
           let t = makeRawTable p in
           (H.toHtml $  UP.printarg p, PH.makeTable t)

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




