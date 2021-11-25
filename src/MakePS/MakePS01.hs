{-# LANGUAGE OverloadedStrings #-}

-- pl translations

module MakePS.MakePS01 (mkps01g,mkps01string,mkps01html) where

import Text.LaTeX
import Text.LaTeX.Base.Commands
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.Geometry
import Text.LaTeX.Packages.Inputenc   -- usepackage [utf8] inputenc
import Text.LaTeX.Base.Pretty         -- prettyLaTeX
import Text.LaTeX.Packages.Trees.Qtree
import Text.LaTeX.Packages.AMSSymb
import Text.LaTeX.Base.Math
import qualified Data.String as S
import Data.List
import System.Random
import Control.Monad

import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H5

import Translations.RandomSentences

-- | for integration with problemsets online, we make html with embedded latex
mkps01html :: IO H.Html
mkps01html = do
       g <- newStdGen -- get random generator
       let (num,_) = next g
       let seed = mkStdGen num
       let (g1,g2) = split seed
       let (q1q,q1a) = pltranslationg g1
       let (q2q,q2a) = pltranslationg g2
       let questionstring = prettyLaTeX (ps01q (q1q,q2q) num)
       let answerstring = prettyLaTeX (ps01a (q1q,q1a) (q2q,q2a) num)
       let (q1qh,q1gh,q1th) = pltranslationgStr g1
       let (q2qh,q2gh,q2th) = pltranslationgStr g2
       return (htmltemplate $ QandASet q1qh q2qh q1gh q2gh q1th q2th questionstring answerstring)

data QandASet = QandASet {htmlQ1 :: String
                         ,htmlQ2 :: String
                         ,htmlQG1 :: String
                         ,htmlQG2 :: String
                         ,htmlTG1 :: String
                         ,htmlTG2 :: String
                         ,latexQS :: String
                         ,latexQAS :: String} deriving (Show)

htmltemplate :: QandASet -> H.Html
htmltemplate qa = do
       H5.h1 $ H.toHtml ("Problem Set 1: Translations from English into PL" :: String)
       H5.h2 $ H.toHtml ("Just the Questions" :: String)
       H5.p $ H.toHtml ("Q1. Translate the following into PL. Provide a glossary for your translation." :: Text)
       H5.p $ H.toHtml (htmlQ1 qa)
       H5.p $ H.toHtml ("Q2. Translate the following into PL. Provide a glossary for your translation." :: Text)
       H5.p $ H.toHtml (htmlQ2 qa)
       H5.h2 $ H.toHtml ("Questions and Answers" :: String)
       H5.p $ H.toHtml ("Q1. Translate the following into PL. Provide a glossary for your translation." :: Text)
       H5.p $ H.toHtml (htmlQ1 qa)
       H5.p $ H.toHtml ("Glossary:" :: String)
       H5.p $ mconcat $ intersperse H5.br $ map H.toHtml (S.lines (htmlQG1 qa))
       H5.p $ H.toHtml ("Translation:" :: String)
       H5.p $ H.toHtml (htmlTG1 qa)
       H5.p $ H.toHtml ("Q2. Translate the following into PL. Provide a glossary for your translation." :: Text)
       H5.p $ H.toHtml (htmlQ2 qa)
       H5.p $ H.toHtml ("Glossary:" :: String)
       H5.p $ mconcat $ intersperse H5.br $ map H.toHtml (S.lines (htmlQG2 qa))
       H5.p $ H.toHtml ("Translation:" :: String)
       H5.p $ H.toHtml (htmlTG2 qa)
       H5.h2 $ H.toHtml ("Just the Questions (LaTeX)" :: String)
       H5.p $ H.toHtml (latexQS qa)
       H5.h2 $ H.toHtml ("Questions and Answers (LaTeX)" :: String)
       H5.p $ H.toHtml (latexQAS qa)


-- | just give me a string man!
mkps01string :: IO (String, String)
mkps01string = do
       g <- newStdGen    -- get random generator
       let (num,_) = next g  -- use it to get a random number
       let seed = mkStdGen num
       let (g1,g2) = split seed
       let (q1q,q1a) = pltranslationg g1
       let (q2q,q2a) = pltranslationg g2
       let questionstring = prettyLaTeX (ps01q (q1q,q2q) num)
       let answerstring = prettyLaTeX (ps01a (q1q,q1a) (q2q,q2a) num)
       return (questionstring,answerstring)


-- |GENERAL DOCUMENT BUILDING FUNCTIONS

-- |function to render questions and answers to .tex file
mkps01g :: RandomGen g => g -> Int -> IO ()
mkps01g g n  = do
         let (q1q,q1a) = pltranslationg g1
         let (q2q,q2a) = pltranslationg g2
         renderFile ("ps01" ++ "-" ++ show n ++ "q.tex") (ps01q (q1q,q2q) n) -- render questions to tex
         renderFile ("ps01" ++ "-" ++ show n ++ "a.tex") (ps01a (q1q,q1a) (q2q,q2a) n) -- render answers to tex
              where (g1,g2) = split g



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




