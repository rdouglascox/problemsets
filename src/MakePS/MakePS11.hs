{-# LANGUAGE OverloadedStrings #-}

-- | gpli translations

module MakePS.MakePS11 (mkps11g,mkps11string,mkps11html) where

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

import NewTranslations.TranslationsQandA

import Translations.RandomSentences


import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H5
import qualified Data.String as S

-- | html output

mkps11html :: IO H.Html
mkps11html = do
       g <- newStdGen    -- get random generator
       let (num,_) = next g  -- use it to get a random number
       let seed = mkStdGen num
       let (q,qa) = gplitrans seed
       let (qh,qah) = gplitransh seed
       let questionstring = prettyLaTeX (ps11q q num)
       let answerstring = prettyLaTeX (ps11a qa num)
       return (htmltemplate $ QandASet qh qah questionstring answerstring)

data QandASet = QandASet {htmlQ1 :: H.Html
                         ,htmlA1 :: H.Html
                         ,latexQS :: String
                         ,latexQAS :: String}

htmltemplate :: QandASet -> H.Html
htmltemplate qa = do
       H5.h1 $ H.toHtml ("Problem Set 11: Translations from English into GPLI" :: String)
       H5.h2 $ H.toHtml ("Just the Questions" :: String)
       H5.p $ H.toHtml ("Q1. Translate the following into GPL. Provide a glossary for your translation." :: Text)
       H5.p $ H.toHtml (htmlQ1 qa)
       H5.h2 $ H.toHtml ("Questions and Answers" :: String)
       H5.p $ H.toHtml ("Q1. Translate the following into GPL. Provide a glossary for your translation." :: Text)
       H5.p $ H.toHtml (htmlA1 qa)
       H5.h2 $ H.toHtml ("Just the Questions (LaTeX)" :: String)
       H5.p $ H.toHtml (latexQS qa)
       H5.h2 $ H.toHtml ("Questions and Answers (LaTeX)" :: String)
       H5.p $ H.toHtml (latexQAS qa)

-- | just give me a string man!
mkps11string :: IO (String, String)
mkps11string = do
       g <- newStdGen    -- get random generator
       let (num,_) = next g  -- use it to get a random number
       let seed = mkStdGen num
       let (q,qa) = gplitrans seed
       let questionstring = prettyLaTeX (ps11q q num)
       let answerstring = prettyLaTeX (ps11a qa num)
       return (questionstring,answerstring)
   
-- |GENERAL DOCUMENT BUILDING FUNCTIONS

-- |function to render questions and answers to .tex file
mkps11g :: RandomGen g => g -> Int -> IO ()
mkps11g g n  = do
         let (q,qa) = gplitrans g
         renderFile ("ps11" ++ "-" ++ show n ++ "q.tex") (ps11q q n) -- render questions to tex
         renderFile ("ps11" ++ "-" ++ show n ++ "a.tex") (ps11a qa n) -- render answers to tex
              where (g1,g2) = split g

-- |document preamble

-- |preamble for questions
ps11pq :: Int -> LaTeX 
ps11pq n = docSettings <> title "Problem Set 11: GPLI Translations (Questions)" <> author "" <> date (fromString $ "#" ++ show n)

-- |preamble for answers
ps11pa :: Int -> LaTeX 
ps11pa n = docSettings <> title "Problem Set 11: GPLI Translations (Answers)" <> author "" <> date (fromString $ "#" ++ show n)

-- |shared document settings
docSettings :: LaTeX
docSettings = documentclass [] article 
            <> usepackage [] amssymb 
            <> usepackage [utf8] inputenc 
            <> usepackage [] qtree 
            <> importGeometry [GWidth (Cm 18)]

-- |final latex document to render

-- |only questions
ps11q :: LaTeX -> Int ->  LaTeX
ps11q x n = ps11pq n <> document (maketitle <> questions x)

-- |with answers
ps11a :: LaTeX -> Int -> LaTeX
ps11a x n = ps11pa n <> document (maketitle <> answers x)

-- |DOCUMENT BODY

-- |shared

-- | the text of q1
q11text :: LaTeX
q11text = item Nothing <> "Translate the following into MPL. Provide a glossary for your translation. (If the sentence is ambiguous, then provide a translation for every disambiguation of the sentence)."


-- |template for just the questions

-- |question 1 question template, expects the question in LaTeX
q11qtemp :: LaTeX -> LaTeX
q11qtemp q = q

q11atemp :: LaTeX -> LaTeX
q11atemp q = q

-- |template for questions and answers

-- |only questions template -- expects each question in order
questions :: LaTeX -> LaTeX
questions q = enumerate (q11text <> q11qtemp q)

-- |questions and answers template -- expects question and answer pairs
answers :: LaTeX -> LaTeX
answers qa = enumerate (q11text <> q11atemp qa)




