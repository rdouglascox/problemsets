{-# LANGUAGE OverloadedStrings #-}


module Printing.HTMLGPLITrees (printtree) where

import Printing.UnicodeGPLIProps
import Data.GPLITree
import Printing.RenderSVGGPLI
import qualified Text.Blaze.Html as H

printtree :: PTree -> IO H.Html
printtree t = do
    a <- renderTree t
    return (H.preEscapedToMarkup a)


 
