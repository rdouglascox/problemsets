module Printing.HTMLGPLIModel (printmodels, printmodel) where


import Trees.GPLItrees
import Data.GPLIModel
import Data.List

import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H5

-- | straight to H.Html from models, without going via TextGPLIModel
printmodels :: [Model] -> H.Html
printmodels ms = mconcat $ intersperse (H5.br <> H5.br) (map printmodel' ms)

printmodel = printmodel'

printmodel' :: Model -> H.Html
printmodel' (Model dom refs exts) = domline dom <> H5.br <> refslines refs <> H5.br <> extslines exts

domline :: [Int]-> H.Html
domline x = H.toHtml "Domain: " <> lbrace <> mconcat (intersperse (H.toHtml ", ") (map (H.toHtml . show) x)) <> rbrace

refslines :: [(Char,Int)] -> H.Html
refslines [] = H.toHtml ""
refslines x = H.toHtml "Referents: " <> mconcat (intersperse (H.toHtml ", ") (refslist x))

refslist :: [(Char, Int)] -> [H.Html]
refslist [] = []
refslist ((c,i):xs)= H.toHtml [c] <> H.toHtml ": " <> H.toHtml (show i) : refslist xs

-- | returns a H.Html representation of the extensions of predicates
extslines :: [(Char,[[Int]])] -> H.Html
extslines [] = H.toHtml ("" :: String)
extslines x = H.toHtml ("Extensions: " :: String) <> mconcat (intersperse (H.toHtml (", " :: String)) (extslist x))

-- | returns a list of H.Html representations of the form 'F: {<1,2>}' for example
extslist :: [(Char, [[Int]])] -> [H.Html]
extslist [] = []
extslist ((c,is):xs)= H.toHtml [c] <> H.toHtml (": " :: String) <> predlists is : extslist xs

-- | returns the relevant H.Html representation for extensions. what we should expect from models
-- | is a list of lists of length n. 

predlists :: [[Int]] -> H.Html
predlists [] = lbrace <> H.toHtml "" <> rbrace -- empty extension case
predlists (x:xs) = if length x == 1
    then lbrace <> mconcat (intersperse (H.toHtml ", ") $ map (H.toHtml . show) (concat (x:xs))) <> rbrace
    else lbrace <> mconcat (intersperse (H.toHtml ", ") (dotups (x:xs))) <> rbrace

-- | here we do the tuples. 
dotups :: [[Int]] -> [H.Html]
dotups [] = []
dotups (x:xs) = langle<> mconcat ( intersperse (H.toHtml ", ") (map (H.toHtml . show) x)) <> rangle : dotups xs

lbrace :: H.Html
lbrace = H.toHtml "{"
rbrace :: H.Html
rbrace = H.toHtml "}"
langle :: H.Html
langle = H.toHtml "⟨"
rangle :: H.Html
rangle = H.toHtml "⟩"