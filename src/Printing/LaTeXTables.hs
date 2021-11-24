{-# LANGUAGE OverloadedStrings #-}

-- Convert a Prop to its LaTeX representation  
-- Modify the definitions for the latex commands for connectives for
-- different outputs

module Printing.LaTeXTables (makeTable) where

import Text.LaTeX.Base
import Text.LaTeX.Base.Commands
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Math
import Printing.LaTeXPLProps
import Data.PLprop
import Tables.Tables
import Data.List

-- CenterColumn, VerticalLine, DVerticalLine

makeTable :: RawTable -> LaTeX
makeTable (mh,bh,m,b) = tabular Nothing spec content
    where spec = makeSpec (mh,bh)
          content = hline <> makeHeaderRow (mh,bh) <> hline <> makeBodyRows (m,b) <> hline

makeSpec :: ([Prop],[Prop]) -> [TableSpec]
makeSpec (mh,bh) = [VerticalLine] <> (intersperse VerticalLine [CenterColumn | x <- mh]) <> [DVerticalLine] <> (intersperse VerticalLine [CenterColumn | x <- bh]) <> [VerticalLine]

makeHeaderRow :: ([Prop],[Prop]) -> LaTeX
makeHeaderRow (mh,bh) = foldl1 (&) (map printprop (mh ++ bh)) <> tabularnewline   

unify [] [] = []
unify (x:xs) (y:ys) = (x ++ y): unify xs ys 

makeBodyRows :: ([[Bool]],[[Bool]]) -> LaTeX
makeBodyRows (m,b) = mconcat $ map makeRow (unify m b)

makeRow :: [Bool] -> LaTeX 
makeRow bs = foldl1 (&) (map printBool bs) <> tabularnewline 

printBool :: Bool -> LaTeX
printBool True = fromString "T"
printBool False = fromString "F" 
