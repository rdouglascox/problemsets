module NewTranslations.Glossary (getglossary) where

import Data.GPLIprop
import Data.Char

-- | produce a glossary from a translation (from a gpli expression)

getnames :: Prop -> String
getnames p = case p of
  Atomic pred tes -> case tes of
    [] -> []
    te : tes' -> case te of
      Variable c -> fromterms tes'
      Constant c -> c : fromterms tes'
  Negation pr -> getnames pr
  Existential c pr -> getnames pr
  Universal c pr -> getnames pr
  Conjunction pr pr' -> getnames pr ++ getnames pr'
  Disjunction pr pr' -> getnames pr ++ getnames pr'
  Conditional pr pr' -> getnames pr ++ getnames pr'
  Biconditional pr pr' -> getnames pr ++ getnames pr'
  where fromterms [] = []
        fromterms (Variable x:xs) = fromterms xs
        fromterms (Constant x:xs) = x : fromterms xs

getpreds :: Prop -> [(Char,Int)]
getpreds p = case p of
  Atomic (Predicate pred) tes -> [(pred,length tes)]
  Negation pr -> getpreds pr
  Existential c pr -> getpreds pr
  Universal c pr -> getpreds pr
  Conjunction pr pr' -> getpreds pr ++ getpreds pr'
  Disjunction pr pr' -> getpreds pr ++ getpreds pr'
  Conditional pr pr' -> getpreds pr ++ getpreds pr'
  Biconditional pr pr' -> getpreds pr ++ getpreds pr'

names :: [String]
names = ["Ashley","Blaire","Clem","Danny","Eden","Frances","Glenn","Harley","Indigo","Jamie","Kris","Luca","Morgan","Natt","Parker","Reagan","Sam","Taylor","Vick","Winter","Zane"]

namedict :: [(Char, String)]
namedict = zip (map (toLower . head) names) (glossbit names)

glossbit :: [String] -> [String]
glossbit = map (\ x -> toLower (head x) : (" : " ++ x))

verbdict :: [((Char, Int), String)]
verbdict = [(('R',1),"Rx : x is running")
            ,(('J',1),"Jx : x is jumping" )
            ,(('C',1),"Cx : x is clapping" )
            ,(('D',1), "Dx : x is drumming" )
            ,(('E',1), "Ex : x is escaping" )
            ,( ('F',1), "Fx : x is frightened" )
            ,(('M',1), "Mx : x is mapmaking")
            ,(('Q',1), "Qx : x is quiltmaking" )
            ,(('H',1), "Hx : x is happy" )
            ,(('S',1), "Sx : x is sad" )
            ,(('K',1), "Kx : x is keen" )
            ,(('L',2), "Lxy : x loves y" )
            ,(('H',2), "Hxy : x hates y" )
            ,(('N',2), "Nxy : x needs y" )
            ,(('U',2), "Uxy : x understands y")
            ,(('V',1), "Vx : x is vacillating" )
            ,(('P',1), "Px : x is a person" )
            ,(('W',1), "Wx : x is waiting" )
            ,(('X',1), "Xx : x is zylophoning" )
            ,(('Y',1), "Yx : x is yelling" )
            ,(('Z',1), "Zx : x is ziplining" )
            ]

doverbgloss :: [(Char,Int)] -> [((Char, Int), String)] -> [String]
doverbgloss xs [] = []
doverbgloss xs ((x,str):ys) = if x `elem` xs
    then str : doverbgloss xs ys
    else doverbgloss xs ys

getverbgloss :: Prop -> [String]
getverbgloss p =  doverbgloss (getpreds p) verbdict

donamegloss :: String -> [(Char, String)] -> [String]
donamegloss xs [] = []
donamegloss xs ((x,str):ys) = if x `elem` xs
    then str : donamegloss xs ys
    else donamegloss xs ys

getnamegloss :: Prop -> [String]
getnamegloss p = donamegloss (getnames p) namedict

getglossary :: Prop -> [String]
getglossary p = getnamegloss p ++ getverbgloss p