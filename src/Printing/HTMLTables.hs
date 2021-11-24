{-# LANGUAGE OverloadedStrings #-}

module Printing.HTMLTables (makeTable) where

import Printing.UnicodePLProps ( printprop )
import Tables.Tables ( RawTable )
import Control.Monad ( forM_ )

import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H5

-- | take a raw table and turn it into an html table
makeTable :: RawTable -> H.Html
makeTable (mh,bh,m,b) = let mhhtml = map (H.toHtml . printprop) mh in
                        let bhhtml = map (H.toHtml . printprop) bh in
                        let mhtml = map (map (H.toHtml . printBool)) m in
                        let bhtml = map (map (H.toHtml . printBool)) b in
                        let header = mhhtml <> bhhtml in
                        let body = unify mhtml bhtml in
                        H5.p (H.toHtml ("Table: " :: String)) <> (H5.p $ H5.table $ H5.tr (mapM_ H5.th header) <> forM_ body (H5.tr . mapM_ H5.td))

unify :: [[a]] -> [[a]] -> [[a]]
unify [] [] = []
unify (x:xs) (y:ys) = (x ++ y) : unify xs ys

printBool :: Bool -> String
printBool True = "T"
printBool False = "F"
