module Forms.PLtrees where

-- this module is just a helper module for the website, it includes the relevant functions for parsing and displaying trees on forms

-- the function should take text as input, and HTML. this keeps most of the heavy lifting outside of the webapp code

import Parsers.PLToken1
import Parsers.PLParser

import Data.PLprop
import Trees.PLtreesNew 
import qualified Data.Text as T
import Printing.HTMLPLTrees
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H5
import Data.List

parsePL :: String -> Maybe Prop
parsePL = happyParser . alexScanTokens

splitInput :: T.Text -> [String]
splitInput i = map T.unpack (T.splitOn (T.pack ",") i)

treeformHTML :: T.Text -> IO H.Html
treeformHTML t = do
    let proplist = mapM parsePL (splitInput t)
    case proplist of
        Nothing -> return $ H5.p (H.toHtml "Invalid input. Try again.")
        Just ps -> let mtree = mktreeSafe ps in
            case mtree of
                Nothing -> return $ H5.p (H.toHtml "Sorry, that tree got too big.")
                Just t -> printtree t


treeformHTMLa :: T.Text -> IO H.Html
treeformHTMLa t = do
    let proplist = mapM parsePL (splitInput t)
    case proplist of
        Nothing -> return $ H5.p (H.toHtml "Invalid input. Try again.")
        Just ps -> let mtree = mktreeSafe (toarg ps) in
            case mtree of
                Nothing -> return $ H5.p (H.toHtml "Sorry, that tree got too big.")
                Just t -> printtree t

toarg :: [Prop] -> [Prop]
toarg xs = init xs ++ [Negation $ last xs]