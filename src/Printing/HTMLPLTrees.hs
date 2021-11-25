{-# LANGUAGE OverloadedStrings #-}


module Printing.HTMLPLTrees (printtree) where

import Printing.UnicodePLProps
import Data.PLprop
import qualified Trees.PLtrees as L
import Printing.RenderSVG
import qualified Text.Blaze.Html as H

printtree :: L.Tree [L.TProp] -> IO H.Html
printtree t = do
    a <- renderTree t
    return (H.preEscapedToMarkup a)


 
