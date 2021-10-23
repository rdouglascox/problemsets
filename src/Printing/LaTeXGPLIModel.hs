{-# LANGUAGE OverloadedStrings #-}

module Printing.LaTeXGPLIModel (printmodel,printmodellns,printmodels) where

import qualified Printing.TextGPLIModel as T
import Trees.GPLItrees
import Data.GPLIModel
import Text.LaTeX
import Text.LaTeX.Packages.Geometry
import Text.LaTeX.Packages.Inputenc
import Text.LaTeX.Base
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Math
import Text.LaTeX.Base.Pretty
import Text.LaTeX.Packages.Trees.Qtree
import Text.LaTeX.Packages.AMSSymb
import Data.List

printmodel :: Model -> LaTeX
printmodel m = fromString $ T.printmodel m

printmodellns :: Model -> LaTeX
printmodellns m = mconcat $ intersperse newline $ map fromString $ T.printmodellns m

printmodels :: [Model] -> LaTeX
printmodels ms = mconcat $ intersperse (newline <> newline) $ map printmodel' ms

-- | straight to LaTeX from models, without going via TextGPLIModel

printmodel' :: Model -> LaTeX
printmodel' (Model dom refs exts) = domline dom <> newline <> refslines refs <> newline <> extslines exts

domline :: [Int]-> LaTeX
domline x = fromString "Domain: " <> math (autoBraces (mconcat $ intersperse (fromString ", ") $ map (fromString . show) x))

refslines :: [(Char,Int)] -> LaTeX
refslines [] = fromString ""
refslines x = fromString "Referents: " <> math (mconcat (intersperse (fromString ", ") (refslist x)))

refslist :: [(Char, Int)] -> [LaTeX]
refslist [] = []
refslist ((c,i):xs)= fromString [c] <> fromString ": " <> fromString (show i) : refslist xs

-- | returns a latex representation of the extensions of predicates
extslines :: [(Char,[[Int]])] -> LaTeX
extslines [] = fromString ""
extslines x = fromString "Extensions: " <> math (mconcat (intersperse (fromString ", ") (extslist x)))

-- | returns a list of latex representations of the form 'F: {<1,2>}' for example
extslist :: [(Char, [[Int]])] -> [LaTeX]
extslist [] = []
extslist ((c,is):xs)= fromString [c] <> fromString ": " <> predlists is : extslist xs

-- | returns the relevant latex representation for extensions. what we should expect from models
-- | is a list of lists of length n. 

predlists :: [[Int]] -> LaTeX
predlists [] = autoBraces (fromString "") -- empty extension case
predlists (x:xs) = if length x == 1
    then autoBraces (mconcat $ intersperse (fromString ", ") $ map (fromString . show) (concat (x:xs)))
    else autoBraces (mconcat $ intersperse (fromString ", ") (dotups (x:xs)))

-- | here we do the tuples. 
dotups :: [[Int]] -> [LaTeX]
dotups [] = []
dotups (x:xs) = autoAngleBrackets (mconcat $ intersperse (fromString ", ") $ map (fromString . show) x) : dotups xs