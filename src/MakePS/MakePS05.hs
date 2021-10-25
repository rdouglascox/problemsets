{-# LANGUAGE OverloadedStrings #-}

module MakePS.MakePS05 (mkps05g,mkps05string) where

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

-- | just give me a string man!
mkps05string :: IO (String, String)
mkps05string = do
       g <- newStdGen    -- get random generator
       let (num,_) = next g  -- use it to get a random number
       let seed = mkStdGen num
       let (q,qa) = mpltrans seed
       let questionstring = prettyLaTeX (ps05q q num)
       let answerstring = prettyLaTeX (ps05a qa num)
       return (questionstring,answerstring)
   

-- |GENERAL DOCUMENT BUILDING FUNCTIONS

-- |function to render questions and answers to .tex file
mkps05g :: RandomGen g => g -> Int -> IO ()
mkps05g g n  = do
         let (q,qa) = mpltrans g
         renderFile ("ps05" ++ "-" ++ show n ++ "q.tex") (ps05q q n) -- render questions to tex
         renderFile ("ps05" ++ "-" ++ show n ++ "a.tex") (ps05a qa n) -- render answers to tex
              where (g1,g2) = split g

-- |document preamble

-- |preamble for questions
ps05pq :: Int -> LaTeX 
ps05pq n = docSettings <> title "Problem Set 05: MPL Translations (Questions)" <> author "" <> date (fromString $ "#" ++ show n)

-- |preamble for answers
ps05pa :: Int -> LaTeX 
ps05pa n = docSettings <> title "Problem Set 05: MPL Translations (Answers)" <> author "" <> date (fromString $ "#" ++ show n)

-- |shared document settings
docSettings :: LaTeX
docSettings = documentclass [] article 
            <> usepackage [] amssymb 
            <> usepackage [utf8] inputenc 
            <> usepackage [] qtree 
            <> importGeometry [GWidth (Cm 18)]

-- |final latex document to render

-- |only questions
ps05q :: LaTeX -> Int ->  LaTeX
ps05q x n = ps05pq n <> document (maketitle <> questions x)

-- |with answers
ps05a :: LaTeX -> Int -> LaTeX
ps05a x n = ps05pa n <> document (maketitle <> answers x)

-- |DOCUMENT BODY

-- |shared

-- | the text of q1
q1text :: LaTeX
q1text = item Nothing <> "Translate the following into MPL. Provide a glossary for your translation. (If the sentence is ambiguous, then provide a translation for every disambiguation of the sentence.)"


-- |template for just the questions

-- |question 1 question template, expects the question in LaTeX
q1qtemp :: LaTeX -> LaTeX
q1qtemp q = q

q1atemp :: LaTeX -> LaTeX
q1atemp q = q

-- |template for questions and answers


-- |only questions template -- expects each question in order
questions :: LaTeX -> LaTeX
questions q = enumerate (q1text <> q1qtemp q)

-- |questions and answers template -- expects question and answer pairs
answers :: LaTeX -> LaTeX
answers qa = enumerate (q1text <> q1atemp qa)




