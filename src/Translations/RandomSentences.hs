{-# LANGUAGE OverloadedStrings #-}

module Translations.RandomSentences (qa, qanda, rsentences, pltranslation) where

import Translations.Sentences
import System.Random
import Translations.Verbs
import Translations.Names
import Data.Char
import Data.List

import Text.LaTeX
import Text.LaTeX.Base.Commands
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Packages.Geometry
import Text.LaTeX.Packages.Inputenc   -- usepackage [utf8] inputenc
import Text.LaTeX.Base.Pretty         -- prettyLaTeX
import Text.LaTeX.Packages.Trees.Qtree
import Text.LaTeX.Packages.AMSSymb
import Text.LaTeX.Base.Math

import Translations.MonadicParserLeft
import Printing.LaTeXPLProps


--

pltranslation :: IO (LaTeX,LaTeX)
pltranslation = do
    s <- rSentence <$> newStdGen
    let sentence = fromString $ ((caps . printSentence) s)
    let glossary = mconcat $ intersperse newline $ map fromString $ printGlossary' (makeGlossary' (makeGlossary s))
    let translation = printprop $ monadicLeftParse $ findandreplace' (makeTranslation s) (makeGlossary' (makeGlossary s))
    return (sentence, glossary <> newline <> newline <> translation)

rsentences = do
    s <- rSentence <$> newStdGen
    putStrLn ((caps . printSentence) s)

qanda = do
      s <- rSentence <$> newStdGen
      putStrLn $ "\nSentence: \n\n" ++ ((caps . printSentence) s)
      putStrLn "\nGlossary: \n" 
      mapM_ putStrLn (printGlossary' (makeGlossary' (makeGlossary s)))  
      putStrLn "\nTranslation: \n"
      putStrLn $ findandreplace' (makeTranslation s) (makeGlossary' (makeGlossary s))


qa = do 
    s <- rSentence <$> newStdGen
    return (((caps . printSentence) s), (printGlossary' (makeGlossary' (makeGlossary s))), findandreplace' (makeTranslation s) (makeGlossary' (makeGlossary s)))


caps :: String -> String
caps (x:xs) = (toUpper x) : xs

-- verb phrases

rand :: RandomGen g => g -> [a] -> a
rand gen xs = xs!!(fst(randomR(0,((length xs) -1)) gen)) 

rVerb :: RandomGen g => g -> Verb
rVerb gen = rand gen (map Verb verbs)

rIntransitiveVerbPhrase :: RandomGen g => g -> VerbPhrase
rIntransitiveVerbPhrase gen = IntransitiveVerbPhrase $ rVerb genl
                              where (genl,genr) = split gen 

rNegatedVerbPhrase :: RandomGen g => g -> VerbPhrase
rNegatedVerbPhrase gen = NegatedVerbPhrase $ IntransitiveVerbPhrase $ rVerb genl
                               where (genl,genr) = split gen 

simplevps :: RandomGen g => g -> [VerbPhrase]
simplevps gen = [rIntransitiveVerbPhrase genl, rNegatedVerbPhrase genr]
    where (genl,genr) = split gen

rVerbPhrase :: RandomGen g => g -> VerbPhrase
rVerbPhrase gen = rIntransitiveVerbPhrase gen

rConjunctiveVerbPhrase :: RandomGen g => g -> VerbPhrase
rConjunctiveVerbPhrase gen = ConjunctiveVerbPhrase (rVerbPhrase genl) (rVerbPhrase genr)
                               where (genl,genr) = split gen 

rDisjunctiveVerbPhrase :: RandomGen g => g -> VerbPhrase
rDisjunctiveVerbPhrase gen = DisjunctiveVerbPhrase (rVerbPhrase genl) (rVerbPhrase genr)
                               where (genl,genr) = split gen 

rVerbPhraseAll :: RandomGen g => g -> VerbPhrase
rVerbPhraseAll gen = rand gen1 [rIntransitiveVerbPhrase gen2
                               ,rNegatedVerbPhrase gen3
                               ,rConjunctiveVerbPhrase gen4
                               ,rDisjunctiveVerbPhrase gen5]
                               where (gen5,gen4) = split gen3
                                     (gen3,gen2) = split gen1
                                     (gen1,gen0) = split gen

-- noun phrases


rNounPhrase :: RandomGen g => g -> NounPhrase
rNounPhrase gen = Proper $ rand gen (map ProperNoun names)

-- sentences

rAtomicSentence :: RandomGen g => g -> Sentence
rAtomicSentence gen = AtomicSentence (rNounPhrase genl) (rVerbPhraseAll genr) 
                where (genl,genr) = split gen

rNegatedSentence :: RandomGen g => g -> Sentence
rNegatedSentence gen = NegatedSentence (rAtomicSentence gen)

rAtomicOrNegatedSentence :: RandomGen g => g -> Sentence
rAtomicOrNegatedSentence gen = rand gen sent
                         where sent = [rAtomicSentence genl, rNegatedSentence genr]
                                    where (genl,genr) = split gen 

rConditionalSentence :: RandomGen g => g -> Sentence
rConditionalSentence gen = ConditionalSentence (rAtomicOrNegatedSentence genl) (rAtomicOrNegatedSentence genr)
                         where (genl, genr) = split gen  

rBiconditionalSentence :: RandomGen g => g -> Sentence
rBiconditionalSentence gen = BiconditionalSentence (rAtomicOrNegatedSentence genl) (rAtomicOrNegatedSentence genr)
                         where (genl, genr) = split gen  

rConjunctiveSentence :: RandomGen g => g -> Sentence
rConjunctiveSentence gen = ConjunctiveSentence (rAtomicOrNegatedSentence genl) (rAtomicOrNegatedSentence genr)
                         where (genl, genr) = split gen  

rDisjunctiveSentence :: RandomGen g => g -> Sentence
rDisjunctiveSentence gen = DisjunctiveSentence (rAtomicOrNegatedSentence genl) (rAtomicOrNegatedSentence genr)
                         where (genl, genr) = split gen  

rSentence :: RandomGen g => g -> Sentence
rSentence gen = rand gen [rAtomicSentence gen1, rNegatedSentence gen2, rConditionalSentence gen3, rBiconditionalSentence gen4, rConjunctiveSentence gen5, rDisjunctiveSentence gen6]
    where (gen6,gen5) = split gen4
          (gen4,gen3) = split gen2
          (gen2,gen1) = split gen

-- print functions

printSentence :: Sentence -> String
printSentence (AtomicSentence x y) = printNounPhrase x ++ " " ++ printVerbPhrase y
printSentence (NegatedSentence x) = "it is not the case that " ++ printSentence x
printSentence (ConditionalSentence x y) = "If " ++ printSentence x ++ " then " ++ printSentence y
printSentence (BiconditionalSentence x y) = printSentence x ++ " if and only if " ++ printSentence y
printSentence (ConjunctiveSentence r@(AtomicSentence p (IntransitiveVerbPhrase s)) l@(AtomicSentence y (NegatedVerbPhrase z))) = printSentence r ++ " but " ++ printSentence l
printSentence (ConjunctiveSentence r@(AtomicSentence p (NegatedVerbPhrase s)) l@(AtomicSentence y (IntransitiveVerbPhrase z))) = printSentence r ++ " but " ++ printSentence l
printSentence (ConjunctiveSentence x y) = printSentence x ++ " and " ++ printSentence y
printSentence (DisjunctiveSentence x y) = "Either " ++ printSentence x ++ " or " ++ printSentence y

printNounPhrase :: NounPhrase -> String
printNounPhrase (Proper (ProperNoun x)) = x

printVerbPhrase :: VerbPhrase -> String
printVerbPhrase (IntransitiveVerbPhrase (Verb x)) = "is " ++ x
printVerbPhrase (NegatedVerbPhrase (IntransitiveVerbPhrase (Verb x))) = "is not " ++ x
printVerbPhrase (ConjunctiveVerbPhrase x y) = (printVerbPhrase x) ++ " and " ++ (printVerbPhrase' y)
printVerbPhrase (DisjunctiveVerbPhrase x y) = (printVerbPhrase x) ++ " or " ++ (printVerbPhrase' y)
 
printVerbPhrase' :: VerbPhrase -> String
printVerbPhrase' (IntransitiveVerbPhrase (Verb x)) = x
printVerbPhrase' (NegatedVerbPhrase (IntransitiveVerbPhrase (Verb x))) = "is not " ++ x

--- GLOSSARY

makeGlossary :: Sentence -> [String]
makeGlossary (AtomicSentence x y) = printSentenceG (AtomicSentence x y)
makeGlossary (NegatedSentence x) = makeGlossary x 
makeGlossary (ConditionalSentence x y) = (makeGlossary x) ++ (makeGlossary y) 
makeGlossary (BiconditionalSentence x y) = (makeGlossary x) ++ (makeGlossary y)
makeGlossary (ConjunctiveSentence x y) = (makeGlossary x) ++ (makeGlossary y)
makeGlossary (DisjunctiveSentence x y) = (makeGlossary x) ++ (makeGlossary y)

makeGlossary' :: [String] -> [(Char,String)]
makeGlossary' xs = zip ['A'..'Z'] xs 

printGlossary :: (Char,String) -> String
printGlossary (x,y) = [x] ++ ": " ++ (caps y)

printGlossary' :: [(Char,String)] -> [String]
printGlossary' xs = map printGlossary xs 

--- helpers

printSentenceG :: Sentence -> [String]
printSentenceG (AtomicSentence x y) =  printVerbPhraseG (printNounPhraseG x) y

printNounPhraseG :: NounPhrase -> String
printNounPhraseG (Proper (ProperNoun x)) = x

printVerbPhraseG :: String -> VerbPhrase -> [String]
printVerbPhraseG s (IntransitiveVerbPhrase (Verb x)) = [s ++ " is " ++ x]
printVerbPhraseG s (NegatedVerbPhrase (IntransitiveVerbPhrase (Verb x))) = [s ++ " is " ++ x]
printVerbPhraseG s (ConjunctiveVerbPhrase x y) = [s ++ (printVerbPhraseG' x), s ++ (printVerbPhraseG' y)]
printVerbPhraseG s (DisjunctiveVerbPhrase x y) = [s ++ (printVerbPhraseG' x), s ++ (printVerbPhraseG' y)]
 
printVerbPhraseG' :: VerbPhrase -> String
printVerbPhraseG' (IntransitiveVerbPhrase (Verb x)) = " is " ++ x
printVerbPhraseG' (NegatedVerbPhrase (IntransitiveVerbPhrase (Verb x))) = " is " ++ x

--- Make Translation

makeTranslation :: Sentence -> String
makeTranslation (AtomicSentence x y) = printSentenceT (AtomicSentence x y)
makeTranslation (NegatedSentence x) = "~" ++ (makeTranslation x) 
makeTranslation (ConditionalSentence x y) = "(" ++ (makeTranslation x) ++ "->" ++ (makeTranslation y) ++ ")"
makeTranslation (BiconditionalSentence x y) = "(" ++ (makeTranslation x) ++ "<->" ++ (makeTranslation y) ++ ")"
makeTranslation (ConjunctiveSentence x y) = "(" ++ (makeTranslation x) ++ "&" ++ (makeTranslation y) ++ ")"
makeTranslation (DisjunctiveSentence x y) = "(" ++ (makeTranslation x) ++ "v" ++ (makeTranslation y) ++ ")"

printSentenceT :: Sentence -> String
printSentenceT (AtomicSentence x (NegatedVerbPhrase y)) =  printVerbPhraseT (printNounPhraseT x) (NegatedVerbPhrase y)
printSentenceT (AtomicSentence x y) =  printVerbPhraseT (printNounPhraseT x) y

printNounPhraseT :: NounPhrase -> String
printNounPhraseT (Proper (ProperNoun x)) = x

printVerbPhraseT :: String -> VerbPhrase -> String
printVerbPhraseT s (IntransitiveVerbPhrase (Verb x)) = s ++ " is " ++ x
printVerbPhraseT s (NegatedVerbPhrase (IntransitiveVerbPhrase (Verb x))) = "~" ++ s ++ " is " ++ x
printVerbPhraseT s (ConjunctiveVerbPhrase x y) = "(" ++ (printVerbPhraseT' s x) ++ "&" ++ (printVerbPhraseT' s y) ++ ")"
printVerbPhraseT s (DisjunctiveVerbPhrase x y) = "(" ++ (printVerbPhraseT' s x) ++ "v" ++ (printVerbPhraseT' s y) ++ ")"
 
printVerbPhraseT' :: String -> VerbPhrase -> String
printVerbPhraseT' s (NegatedVerbPhrase (IntransitiveVerbPhrase (Verb x))) = "~" ++ s ++ " is " ++ x
printVerbPhraseT' s (IntransitiveVerbPhrase (Verb x)) = s ++ " is " ++ x

--- find and replace translation and glossary

findandreplace :: String -> (Char,String) -> String  
findandreplace [] y = []
findandreplace x (y,z) = if (take (length z) x) == z
                         then y : (findandreplace (drop (length z) x) (y,z))   
                         else (head x) : findandreplace (tail x) (y,z)

findandreplace' :: String -> [(Char,String)] -> String
findandreplace' x [] = x
findandreplace' x (y:ys) = findandreplace' (findandreplace x y) ys  

 
