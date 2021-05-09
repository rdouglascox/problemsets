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
printmodels ms = mconcat $ intersperse (newline <> newline) $ map printmodellns ms




