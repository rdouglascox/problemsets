module NewTranslations.TranslationsQandA (makeQandA, QandA (..),mpltrans,gpltrans,gplitrans,transtest,mpltransh,gpltransh,gplitransh) where

import NewTranslations.Generators
    ( mplq1,
      mplq2,
      mplq3,
      mplq4,
      mplq5,
      gplq1,
      gplq2,
      gplq3,
      gplq4,
      gplq5,
      gpliq1,
      gpliq2,
      gpliq3,
      gpliq4,
      gpliq5 )
import NewTranslations.Glossary (getglossary)
import NewTranslations.Translate (translate)
import System.Random ( newStdGen, RandomGen, split )
import Data.GPLIprop ( Prop )
import NewTranslations.Print ( printProp )
import Data.Char ( toUpper )
import Text.LaTeX
    ( IsString(fromString), enumerate, item, newline, quote, LaTeX )
import Printing.LaTeXGPLIProps ( printprops )
import Data.List ( intersperse )

import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html5 as H5
import qualified Printing.UnicodeGPLIProps as UP

data QandA = QandA String TranProps GlosStrings

type TranProps = [Prop]
type GlosStrings = [String]

makeQandA :: RandomGen g => g -> (g -> String) -> QandA
makeQandA g f = let prop = f g in
    let prop' = capitalise prop in
    let tprops = translate prop in
        let gstrings = case tprops of
                            [] -> ["oops. something has gone wrong."]
                            xs -> getglossary (head xs) in
               QandA prop' tprops gstrings

capitalise :: String -> String
capitalise [] = ""
capitalise (x:xs) = toUpper x : xs

sampleQandA :: IO ()
sampleQandA = do
    gen <- newStdGen
    let out = makeQandA gen mplq1 in
        putStrLn (printQandA out)

printQandA :: QandA -> String
printQandA (QandA q t g) = "\n" ++ q ++ "\n\n" ++ unlines (map printProp t) ++ "\n" ++ unlines g

-- | produce HTML questions / question and answer pairs (as html lists)

justquestionsh :: [QandA] -> H.Html
justquestionsh x = H5.ol $ mapM_ (H5.li . H.toHtml) (extractquestions x)

questionsandanswersh :: [QandA] -> H.Html
questionsandanswersh x = H5.ol $ mapM_ (H5.li . displayquandah) x

displayquandah :: QandA -> H.Html
displayquandah (QandA q t g) = H5.p (H.toHtml q) <> H5.p (H.toHtml ("Translation: " :: String)) <> H5.p (H.toHtml $ UP.printprops t) <> H5.p (H.toHtml ("Glossary: " :: String)) <> mconcat (intersperse H5.br (map H.toHtml g)) <> H5.br <> H5.br

-- | produce LaTeX questions / question and answer pairs (as itemized)

-- | QandA to just questions

justquestions :: [QandA] -> LaTeX
justquestions x = enumerate $ mconcat $ map ((item Nothing <>). fromString) (extractquestions x)

extractquestions :: [QandA] -> [String]
extractquestions [] = []
extractquestions (QandA q _ _:xs) = q : extractquestions xs

-- | QandA to questions and answers

questionsandanswers :: [QandA] -> LaTeX
questionsandanswers x = enumerate $ mconcat $ map ((item Nothing <>) . displayqanda) x

displayqanda :: QandA -> LaTeX
displayqanda (QandA q t g) = fromString q <> quote (printprops t) <> quote (mconcat (intersperse newline (map fromString g)))

-- | mpl translations

mpltrans :: RandomGen g => g -> (LaTeX,LaTeX)
mpltrans g = let (g1,g2) = split g in
    let (g3,g4) = split g1 in
    let (g5,g6) = split g2 in
    let qanda = [makeQandA g1 mplq1
                ,makeQandA g2 mplq2
                ,makeQandA g3 mplq3
                ,makeQandA g4 mplq4
                ,makeQandA g5 mplq4] in
                    (justquestions qanda,questionsandanswers qanda)

gpltrans :: RandomGen g => g -> (LaTeX,LaTeX)
gpltrans g = let (g1,g2) = split g in
    let (g3,g4) = split g1 in
    let (g5,g6) = split g2 in
    let qanda = [makeQandA g1 gplq1
                ,makeQandA g2 gplq2
                ,makeQandA g3 gplq3
                ,makeQandA g4 gplq4
                ,makeQandA g5 gplq5] in
                    (justquestions qanda,questionsandanswers qanda)

gplitrans :: RandomGen g => g -> (LaTeX,LaTeX)
gplitrans g = let (g1,g2) = split g in
    let (g3,g4) = split g1 in
    let (g5,g6) = split g2 in
    let qanda = [makeQandA g1 gpliq1
                ,makeQandA g2 gpliq2
                ,makeQandA g3 gpliq3
                ,makeQandA g4 gpliq4
                ,makeQandA g5 gpliq5] in
                    (justquestions qanda,questionsandanswers qanda)

mpltransh :: RandomGen g => g -> (H.Html,H.Html)
mpltransh g = let (g1,g2) = split g in
    let (g3,g4) = split g1 in
    let (g5,g6) = split g2 in
    let qanda = [makeQandA g1 mplq1
                ,makeQandA g2 mplq2
                ,makeQandA g3 mplq3
                ,makeQandA g4 mplq4
                ,makeQandA g5 mplq4] in
                    (justquestionsh qanda,questionsandanswersh qanda)

gpltransh :: RandomGen g => g -> (H.Html,H.Html)
gpltransh g = let (g1,g2) = split g in
    let (g3,g4) = split g1 in
    let (g5,g6) = split g2 in
    let qanda = [makeQandA g1 gplq1
                ,makeQandA g2 gplq2
                ,makeQandA g3 gplq3
                ,makeQandA g4 gplq4
                ,makeQandA g5 gplq5] in
                    (justquestionsh qanda,questionsandanswersh qanda)

gplitransh :: RandomGen g => g -> (H.Html,H.Html)
gplitransh g = let (g1,g2) = split g in
    let (g3,g4) = split g1 in
    let (g5,g6) = split g2 in
    let qanda = [makeQandA g1 gpliq1
                ,makeQandA g2 gpliq2
                ,makeQandA g3 gpliq3
                ,makeQandA g4 gpliq4
                ,makeQandA g5 gpliq5] in
                    (justquestionsh qanda,questionsandanswersh qanda)

-- | testing translations

transtest :: RandomGen g => g -> (LaTeX,LaTeX)
transtest g = let (g1,g2) = split g in
    let (g3,g4) = split g1 in
    let (g5,g6) = split g2 in
    let qanda = map (makeQandA g) ttest ++ 
                    map (makeQandA g1) ttest ++ 
                    map (makeQandA g2) ttest ++ 
                    map (makeQandA g3) ttest ++ 
                    map (makeQandA g4) ttest ++ 
                    map (makeQandA g5) ttest ++ 
                    map (makeQandA g6) ttest in
                    (justquestions qanda,questionsandanswers qanda)

ttest :: RandomGen g => [g -> String]
ttest = [mplq1,mplq2,mplq3,mplq4,mplq5,gplq1,gplq2,gplq3,gplq4,gplq5,gpliq1,gpliq2,gpliq3,gpliq4,gpliq5]
