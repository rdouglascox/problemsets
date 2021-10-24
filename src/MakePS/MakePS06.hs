{-# LANGUAGE OverloadedStrings #-}

module MakePS.MakePS06 (mkps06g,mkps06string) where

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
mkps06string :: IO (String, String)
mkps06string = do
       g <- newStdGen    -- get random generator
       let (num,_) = next g  -- use it to get a random number
       let seed = mkStdGen num
       let (q,qa) = mpltrans seed
       let questionstring = prettyLaTeX (ps06q q num)
       let answerstring = prettyLaTeX (ps06a qa num)
       return (questionstring,answerstring)
   

-- |GENERAL DOCUMENT BUILDING FUNCTIONS

-- |function to render questions and answers to .tex file
mkps06g :: RandomGen g => g -> Int -> IO ()
mkps06g g n  = do
         let (q,qa) = gpltrans g
         renderFile ("ps06" ++ "-" ++ show n ++ "q.tex") (ps06q q n) -- render questions to tex
         renderFile ("ps06" ++ "-" ++ show n ++ "a.tex") (ps06a qa n) -- render answers to tex
              where (g1,g2) = split g

-- |document preamble

-- |preamble for questions
ps06pq :: Int -> LaTeX 
ps06pq n = docSettings <> title "Problem Set 06: GPL Translations (Questions)" <> author "" <> date (fromString $ "#" ++ show n)

-- |preamble for answers
ps06pa :: Int -> LaTeX 
ps06pa n = docSettings <> title "Problem Set 06: GPL Translations (Answers)" <> author "" <> date (fromString $ "#" ++ show n)

-- |shared document settings
docSettings :: LaTeX
docSettings = documentclass [] article 
            <> usepackage [] amssymb 
            <> usepackage [utf8] inputenc 
            <> usepackage [] qtree 
            <> importGeometry [GWidth (Cm 18)]

-- |final latex document to render

-- |only questions
ps06q :: LaTeX -> Int ->  LaTeX
ps06q x n = ps06pq n <> document (maketitle <> questions x)

-- |with answers
ps06a :: LaTeX -> Int -> LaTeX
ps06a x n = ps06pa n <> document (maketitle <> answers x)

-- |DOCUMENT BODY

-- |shared

-- | the text of q1
q6text :: LaTeX
q6text = item Nothing <> "Translate the following into MPL. Provide a glossary for your translation. (If the sentence is ambiguous, then provide a translation for every disambiguation of the sentence."


-- |template for just the questions

-- |question 1 question template, expects the question in LaTeX
q6qtemp :: LaTeX -> LaTeX
q6qtemp q = q

q6atemp :: LaTeX -> LaTeX
q6atemp q = q

-- |template for questions and answers


-- |only questions template -- expects each question in order
questions :: LaTeX -> LaTeX
questions q = enumerate (q6text <> q6qtemp q)

-- |questions and answers template -- expects question and answer pairs
answers :: LaTeX -> LaTeX
answers qa = enumerate (q6text <> q6atemp qa)




