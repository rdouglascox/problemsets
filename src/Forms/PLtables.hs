module Forms.PLtables where

-- this module is just a helper module for the website, it includes the relevant functions for parsing and displaying trees on forms

-- the function should take text as input, and return HTML. this keeps most of the heavy lifting outside of the webapp code

import Parsers.PLToken1
import Parsers.PLParser

import Data.PLprop
import Trees.PLtreesNew 

import Tables.Tables

import qualified Data.Text as T
import Printing.HTMLTables
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H5
import Data.List

parsePL :: String -> Maybe Prop
parsePL = happyParser . alexScanTokens

splitInput :: T.Text -> [String]
splitInput i = map T.unpack (T.splitOn (T.pack ",") i)

tableformHTML :: T.Text -> H.Html
tableformHTML t = 
    let proplist = mapM parsePL (splitInput t) in
    case proplist of
        Nothing -> H5.p (H.toHtml "Invalid input. Try again.")
        Just ps -> makeTable (makeRawTable ps)
            

