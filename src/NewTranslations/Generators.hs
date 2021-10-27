module NewTranslations.Generators (mplq1,mplq2,mplq3,mplq4,mplq5,gplq1,gplq2,gplq3,gplq4,gplq5,gpliq1,gpliq2,gpliq3,gpliq4,gpliq5) where

{--
a module for generating sentences 

--}

import System.Random
import NewTranslations.EnglishL.Abs (CNs(CNs))

-- |for generating translation questions

-- |mpl

mplq1:: RandomGen g =>  g -> String
mplq1 = mpl1a
mplq2:: RandomGen g =>  g -> String
mplq2 = mpl1b
mplq3:: RandomGen g =>  g -> String
mplq3 = mpl1c
mplq4:: RandomGen g =>  g -> String
mplq4 = mpl2a
mplq5:: RandomGen g =>  g -> String
mplq5 = mpl2b

-- |gpl

gplq1:: RandomGen g =>  g -> String
gplq1 = gpl1
gplq2:: RandomGen g =>  g -> String
gplq2 = gpl2
gplq3:: RandomGen g =>  g -> String
gplq3 = gpl3
gplq4:: RandomGen g =>  g -> String
gplq4 = gpl3
gplq5:: RandomGen g =>  g -> String
gplq5 = gpl3


-- |gpli

gpliq1:: RandomGen g =>  g -> String
gpliq1 = gpli1
gpliq2:: RandomGen g =>  g -> String
gpliq2 = gpli2
gpliq3:: RandomGen g =>  g -> String
gpliq3 = gpli3
gpliq4:: RandomGen g =>  g -> String
gpliq4 = gpli4
gpliq5:: RandomGen g =>  g -> String
gpliq5 = gpli4




-- |Get a random element from a list
r :: RandomGen g => g -> [a] -> a
r g x = x!!fst(randomR(0,length x -1) g)

-- | lexical

rnames :: RandomGen g => g -> String
rnames g = r g names

names :: [String]
names = ["Ashley","Blaire","Clem","Danny","Eden","Frances","Glenn","Harley","Indigo","Jamie","Kris","Luca","Morgan","Natt","Parker","Reagan","Sam","Taylor"]

rintverbs :: RandomGen g => g -> String
rintverbs g = r g intverbs

intverbs :: [String]
intverbs = ["running"
            ,"jumping"
            ,"clapping"
            ,"drumming"
            ,"escaping"
            ,"frightened"
            ,"mapmaking"
            ,"quiltmaking"
            ,"vacillating"
            ,"waiting"
            ,"xylophoning"
            ,"yelling"
            ,"ziplining"]


radjs :: RandomGen g => g -> String
radjs g = r g adjs

adjs :: [String]
adjs = ["happy", "sad","keen"]

rtransverbsS  :: RandomGen g => g -> String
rtransverbsS g = r g transverbsS

transverbsS :: [String]
transverbsS = ["loves", "hates", "needs", "understands"]

rtransverbsP  :: RandomGen g => g -> String
rtransverbsP g = r g transverbsP

transverbsP :: [String]
transverbsP = ["love","hate","need","understand"]

rcnS :: RandomGen g => g -> String
rcnS g = r g cnS

cnS :: [String]
cnS = ["boy","girl","onlooker","traveller"]

rcnP :: RandomGen g => g -> String
rcnP g = r g snP

snP :: [String]
snP = ["boys","girls","onlookers","travellers"]

-- | atomics (MLP)

rclause :: RandomGen g =>  g -> String
rclause g = r g [clauseS
                ,clauseP
                ] g

rclause4 :: RandomGen g =>  g -> String
rclause4 g = r g [clauseS1
                , clauseS2
                , clauseS3
                , clauseP1
                ] g


conjnames1 :: RandomGen g => g -> String
conjnames1 g = let (g1,g2) = split g in
    r g [rnames g1 ++ " and " ++ rnames g2
        ,"both " ++ rnames g1 ++ " and " ++ rnames g2]

-- | random intransitive verb phrase
intvp :: RandomGen g => g -> String
intvp g = r g [rintverbs,radjs] g

intvp' :: RandomGen g => g -> String
intvp' g = r g [rintverbs] g

-- | random transitive verb phrase (sing subj, non-quant object)
trnvpS1 :: RandomGen g => g -> String
trnvpS1 g = let (g1,g2) = split g in
    let rn = rnames g1 in
    let rv = rtransverbsS g2 in
        rv ++ " " ++ rn

-- | random transitive verb phrase (sing subj, quant object)
trnvpS2 :: RandomGen g => g -> String
trnvpS2 g = let (g1,g2) = split g in
    let rn = qpS1 g1 in
    let rv = rtransverbsS g2 in
        rv ++ " " ++ rn

trnvpS3 :: RandomGen g => g -> String
trnvpS3 g = let (g1,g2) = split g in
    let rn = qpS3 g1 in
    let rv = rtransverbsS g2 in
        rv ++ " " ++ rn


-- | random transitive verb phrase (plural subj, non-quant object)
trnvpP1 :: RandomGen g => g -> String
trnvpP1 g = let (g1,g2) = split g in
    let rn = rnames g1 in
    let rv = rtransverbsP g2 in
        rv ++ " " ++ rn

-- | random intransitive verb phrase in conj
conjvpS :: RandomGen g => g -> String
conjvpS g = let (g1,g2) = split g in
           let a = intvp' g1 in
           let b = intvp' g2 in
               if a == b then conjvpS g1 else
    r g ["is " ++ a ++ " and " ++ b
        ,"is both " ++ a ++ " and " ++ b
        ,"is " ++ a ++ " or " ++ b
        ,"is either " ++ a ++ " or " ++ b
        ,"is neither " ++ a ++ " nor " ++ b
        ,"is not " ++ a ++ " and " ++ b
        ,"is not both " ++ a ++ " and " ++ b
        ,"is not " ++ a ++ " or " ++ b
        ,"isn't " ++ a ++ " and " ++ b
        ,"isn't both " ++ a ++ " and " ++ b
        ,"isn't " ++ a ++ " or " ++ b
        ]

conjvpP :: RandomGen g => g -> String
conjvpP g = let (g1,g2) = split g in
           let a = intvp' g1 in
           let b = intvp' g2 in
               if a == b then conjvpP g1 else
    r g ["are " ++ a ++ " and " ++ b
        ,"are both " ++ a ++ " and " ++ b
        ,"are " ++ a ++ " or " ++ b
        ,"are either " ++ a ++ " or " ++ b
        ,"are neither " ++ a ++ " nor " ++ b
        ,"are not " ++ a ++ " and " ++ b
        ,"are not both " ++ a ++ " and " ++ b
        ,"are not " ++ a ++ " or " ++ b
        ,"aren't " ++ a ++ " and " ++ b
        ,"aren't both " ++ a ++ " and " ++ b
        ,"aren't " ++ a ++ " or " ++ b
        ]

clauseS :: RandomGen g => g -> String
clauseS g = let (g1,g2) = split g in
    rnames g1 ++ " " ++ conjvpS g2

clauseP :: RandomGen g => g -> String
clauseP g = let (g1,g2) = split g in
    conjnames1 g1 ++ " " ++ conjvpP g2

clauseS1 :: RandomGen g => g -> String
clauseS1 g = let (g1,g2) = split g in
    rnames g1 ++ " " ++ trnvpS1 g2

clauseS2 :: RandomGen g => g -> String
clauseS2 g = let (g1,g2) = split g in
    rnames g1 ++ " " ++ trnvpS2 g2

clauseS3 :: RandomGen g => g -> String
clauseS3 g = let (g1,g2) = split g in
    qpS1 g1 ++ " " ++ trnvpS2 g2

clauseP1 :: RandomGen g => g -> String
clauseP1 g = let (g1,g2) = split g in
    conjnames1 g1 ++ " " ++ trnvpP1 g2

clauseQS :: RandomGen g => g -> String
clauseQS g = let (g1,g2) = split g in
    r g [qpS1 g1 ++ " is " ++ intvp g2
        ,qpS2 g1 ++ " is " ++ intvp g2
        ,qpS3 g1 ++ " is " ++ intvp g2]

clauseQP :: RandomGen g => g -> String
clauseQP g = let (g1,g2) = split g in
    r g [qpP1 g1 ++ " are " ++ intvp g2
        ,qpP2 g1 ++ " are " ++ intvp g2]

-- | quantifier phrases

qpS1 :: RandomGen g => g -> String
qpS1 g = let (g1,g2) = split g in
        r g ["everything"
            ,"something"
            ,"nothing"
            ,"everybody"
            ,"somebody"
            ,"nobody"
            ,"everyone"
            ,"someone"
            ,"noone"
            ,"every " ++ rcnS g1
            ,"some " ++ rcnS g1
            ,"no " ++ rcnS g1
            ]

qpP1 :: RandomGen g => g -> String
qpP1 g = let (g1,g2) = split g in
        r g ["all " ++ rcnP g1
            ,"some " ++ rcnP g1
            ,"no " ++ rcnP g1
            ]

qpS2 :: RandomGen g => g -> String
qpS2 g = let (g1,g2) = split g in
        r g ["at most one " ++ rcnS g1
            ,"exactly one " ++ rcnS g1
            ]

qpP2 :: RandomGen g => g -> String
qpP2 g = let (g1,g2) = split g in
        r g ["at least two " ++ rcnP g1
            ,"at most two " ++ rcnP g1
            ,"exactly two " ++ rcnP g1
            ]

qpS3 :: RandomGen g => g -> String
qpS3 g = let (g1,g2) = split g in
        r g ["everybody except " ++ rnames g2
            ,"everyone except " ++ rnames g2
            ,"everything except " ++ rnames g2
            ,"everybody but " ++ rnames g2
            ,"everyone but " ++ rnames g2
            ,"everything but " ++ rnames g2
            ,"something other than " ++ rnames g2
            ,"someone other than " ++ rnames g2
            ,"somebody other than " ++ rnames g2
            ,"some " ++ rcnS g1 ++ " other than " ++ rnames g2
            ,"every " ++ rcnS g1 ++ " except " ++ rnames g2
            ]

-- | complex

get2clauses :: RandomGen g =>  (g -> String) -> g -> (String,String)
get2clauses f g = let (g1,g2) = split g in
    (f g1, f g2)

rcompclause1 :: RandomGen g => (g -> String) -> g -> String
rcompclause1 f g = let (a,b) = get2clauses f g in
    r g [a ++ " and " ++ b
        , a ++ " but " ++ b
        , "if " ++ a ++  " then " ++ b
        , "if " ++ a ++ " " ++ b
        , a ++ " only if " ++ b
        , a ++ " if " ++ b
        , a ++ " if and only if " ++ b
        , a ++ " and " ++ b
        , a ++ " but " ++ b
        , a ++ " or " ++ b
        , "either " ++ a ++  " or " ++ b
        , "it is not the case that " ++ a
        , "it is not true that " ++ a
        , "it is false that " ++ a
        ]

rcompclause1' :: RandomGen g => String -> String -> g -> String
rcompclause1' a b g =
    r g [a ++ " and " ++ b
        , a ++ " but " ++ b
        , "if " ++ a ++  " then " ++ b
        , "if " ++ a ++ " " ++ b
        , a ++ " only if " ++ b
        , a ++ " if " ++ b
        , a ++ " if and only if " ++ b
        , a ++ " and " ++ b
        , a ++ " but " ++ b
        , a ++ " or " ++ b
        , "either " ++ a ++  " or " ++ b
        , "it is not the case that " ++ a
        , "it is not true that " ++ a
        , "it is false that " ++ a
        ]

rcompclause2 :: RandomGen g => (g -> String) -> g -> String
rcompclause2 f g = let (a,b) = get2clauses f g in
    r g  [a ++ " unless " ++ b
        , a ++ " provided that " ++ b
        , a ++ " assuming that " ++ b
        , a ++ " even though " ++ b
        , "unless " ++ a ++ " " ++ b
        , "provided that " ++ a ++ " " ++ b
        , "assuming that " ++ a ++ " " ++ b
        , "even though " ++ a ++ " " ++ b
        ]

rcompclause2' :: RandomGen g => String -> String -> g -> String
rcompclause2' a b g =
    r g  [a ++ " unless " ++ b
        , a ++ " provided that " ++ b
        , a ++ " assuming that " ++ b
        , a ++ " even though " ++ b
        , "unless " ++ a ++ " " ++ b
        , "provided that " ++ a ++ " " ++ b
        , "assuming that " ++ a ++ " " ++ b
        , "even though " ++ a ++ " " ++ b
        ]


-- | tests



-- | MPL GENERATORS

-- | no quantifiers, two connectives x 2

mpl1a :: RandomGen g =>  g -> String
mpl1a g = let (g1,g2) = split g in
          let (g3,g4) = split g2 in
    r g [rcompclause1' (rcompclause1 mpl1a' g1) (mpl1a' g2) g3
        ,rcompclause1' (mpl1a' g2) (rcompclause1 mpl1a' g1) g3]

mpl1a' :: RandomGen g =>  g -> String
mpl1a' g = let (g1,g2) = split g in
    r g [rnames g1 ++ " is " ++ intvp g2
        ]

mpl1b :: RandomGen g =>  g -> String
mpl1b = mpl1b'

mpl1b' :: RandomGen g =>  g -> String
mpl1b' g = let (g1,g2) = split g in
    r g [rnames g1 ++ " " ++ conjvpS g2
        ,conjnames1 g1 ++ " are " ++ intvp g2]

mpl1c :: RandomGen g =>  g -> String
mpl1c g = let (g1,g2) = split g in
    rcompclause2 mpl1c' g1

mpl1c' :: RandomGen g =>  g -> String
mpl1c' g = let (g1,g2) = split g in
    r g [rnames g1 ++ " is " ++ intvp g2
        ]

-- | one quantifier (personal) x 1

mpl2a :: RandomGen g =>  g -> String
mpl2a = mpl2a'

mpl2a' :: RandomGen g =>  g -> String
mpl2a' g = let (g1,g2) = split g in
    qpS1 g1 ++ " is " ++ intvp g2

mpl2b :: RandomGen g =>  g -> String
mpl2b = mpl2b'

mpl2b' :: RandomGen g =>  g -> String
mpl2b' g = let (g1,g2) = split g in
    qpS1 g1 ++ " " ++ conjvpS g2

-- | GPL GENERATORS

-- | no quantifiers, two connectives x 2

gpl1 :: RandomGen g =>  g -> String
gpl1 g = let (g1,g2) = split g in
          let (g3,g4) = split g2 in
    r g [rcompclause1' (rcompclause1 gpl1' g1) (gpl1' g2) g3
        ,rcompclause1' (gpl1' g2) (rcompclause1 gpl1' g1) g3]

gpl1' :: RandomGen g =>  g -> String
gpl1' g = let (g1,g2) = split g in
    r g [rnames g1 ++ " " ++ trnvpS1 g2
        ]

-- | one quantifier (personal) x 1

gpl2 :: RandomGen g =>  g -> String
gpl2 = gpl2'

gpl2' :: RandomGen g =>  g -> String
gpl2' g = let (g1,g2) = split g in
    qpS1 g1 ++ " " ++ trnvpS1 g2

-- | two quantifiers x 2

gpl3 :: RandomGen g =>  g -> String
gpl3 = gpl3'

gpl3' :: RandomGen g =>  g -> String
gpl3' g = let (g1,g2) = split g in
    qpS1 g1 ++ " " ++ trnvpS2 g2


-- | GPLI GENERATORS

-- | impersonal except uni/or exi (with connectives)

gpli1 :: RandomGen g =>  g -> String
gpli1 = gpli1'

gpli1' :: RandomGen g =>  g -> String
gpli1' g = let (g1,g2) = split g in
    qpS3 g1 ++ " " ++ trnvpS1 g2



-- | personal except uno/or exi (with connectives)

gpli2 :: RandomGen g =>  g -> String
gpli2 = gpli2'

gpli2' :: RandomGen g =>  g -> String
gpli2' g = let (g1,g2) = split g in
    qpS3 g1 ++ " " ++ trnvpS3 g2
    
-- | personal except uno/or exi (with connectives)

gpli3 :: RandomGen g =>  g -> String
gpli3 = gpli3'

gpli3' :: RandomGen g =>  g -> String
gpli3' g = let (g1,g2) = split g in
    qpS2 g1 ++ " " ++ trnvpS1 g2

gpli4 :: RandomGen g =>  g -> String
gpli4 = gpli4'

gpli4' :: RandomGen g =>  g -> String
gpli4' g = let (g1,g2) = split g in
    qpP2 g1 ++ " " ++ trnvpP1 g2
