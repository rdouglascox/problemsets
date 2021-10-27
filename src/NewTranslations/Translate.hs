module NewTranslations.Translate (translate) where

import NewTranslations.EnglishL.Abs
    ( Adj(..),
      AdjPhrase(AdjPNegDisj1, AdjP1, AdjPConj1, AdjPConj2, AdjPDisj1,
                AdjPDisj2),
      CN(..),
      CNs(..),
      Clause(..),
      IVerb(..),
      InvClause(..),
      NQuantPP(..),
      NQuantPS(..),
      Noun(..),
      NounPhraseP(..),
      NounPhraseS(..),
      QuantPP(..),
      QuantPS(..),
      TVerb(..),
      VerbPhrase(..),
      VerbPhraseP(..),
      VerbPhraseS(..),
      VerbX(..) )
import Data.GPLIprop ( Predicate(Predicate), Prop(..), Term(..) )
import Data.Maybe ( fromJust )
import Data.Char ( toLower )
import NewTranslations.JustParses ( justParses )

-- this module needs a lot of documentation to keep track of. basically, we are expecting
-- output from the parser which is a little too fine-grained. the parser, for instance
-- draws a distinction between 'John is not running' and 'John isn't running'. we shouldn't
-- really care about the difference. so we translate from the output of the parser into
-- an intermediate representation that contains all of the information we need for the 
-- final translation into the logical language.

-- the function `translate` is the only function we export from this module. it takes a string
-- representation of the English sentence to translate and then returns a list of propositions
-- representing the translation of the sentence. 

-- | translate EnglishL to GPLI
translate :: String -- ^string representation of EnglishL sentence to translate 
  -> [Prop] -- ^list of propositions representing the translations of the sentence
translate xs = case justParses xs of
  [] -> []
  cl : cls -> concatMap tt (justParses xs)

-- | convert failure represented by either into failure represented by empty list
tt :: Clause -> [Prop]
tt c = case buildLClause c of
  Left s -> []
  Right lc -> case tClause lc of
      Left s -> []
      Right c -> c

-- phase 1: conversion to intermediate format

-- the datatype `LClause` represents the main format we want to translate the output of the
-- parser into. the most important constructor here is `LClause` itself. this represents
-- the basic case: a clause with one predicate, a subject quantifier, an object quantifier,
-- or both, and a list of terms (variables and constants).

data LClause = LClause Valance [PosQuant] Pred [Trm]
             | LClauseConjunction Valance LClause LClause
             | LClauseDisjunction Valance LClause LClause
             | LClauseConditional LClause LClause
             | LClauseBiConditional LClause LClause
             | LClauseNegation LClause
             | LClauseConjunction' Valance [PosQuant] (Pred,Pred) [Trm]
             | LClauseDisjunction' Valance [PosQuant] (Pred,Pred) [Trm]
             | LClauseExists Quants

data Valance = Pos | Neg
data PosQuant = SubQuant Quants | ObjQuant Quants
data Quants = Uni [Restriction] [Exclusion]
            | Exi [Restriction] [Exclusion]
            | NegExi [Restriction] [Exclusion]
            | AtLeast2 [Restriction] [Exclusion]
            | AtLeast3 [Restriction] [Exclusion]
            | AtMost1 [Restriction] [Exclusion]
            | AtMost2 [Restriction] [Exclusion]
            | AtMost3 [Restriction] [Exclusion]
            | Exactly1 [Restriction] [Exclusion]
            | Exactly2 [Restriction] [Exclusion]
            | Exactly3 [Restriction] [Exclusion]
newtype Exclusion = Exclusion String
newtype Restriction = Restriction String
newtype Pred = Pred String
data Trm = Const String | Var Char

data LClause' = LClause' ArgElement PredElement

-- | noun phrase stuff

-- noun phrase return type. needs to register: is it quantificational? if so, what quantifier?
-- is it in subject position or object position? is it single noun, or two nouns, what are they,
-- how are they connected? we should be able to ignore the difference here between plural and 
-- singular rules

data ArgElement = ArgElement ArgType (Either Quants NameArgs)

data ArgType = Subject | Object
data NameArgs = NPSingle Trm
              | NPOr Trm Trm
              | NPAnd Trm Trm
              | NPNegOr Trm Trm

-- | build an `ArgElement` from an argtype (Subject | Object) and a singular noun phrase
nps :: ArgType -> NounPhraseS -> ArgElement
nps arg np = case np of
  NPS1 no -> ArgElement arg (Right (NPSingle (Const $ fromnoun no)))
  NPS2 qp -> ArgElement arg (Left (getquantS1 qp))
  NPS3 nqp -> ArgElement arg (Left (getnumquantsS1 nqp))

fromnoun :: Noun -> String
fromnoun (Noun st) = st

-- | get singular non-numerical quantifiers
getquantS1 :: QuantPS -> Quants
getquantS1 qp = case qp of
    QuantPS1 -> Uni [] []
    QuantPS2 -> Exi [] []
    QuantPS3 -> NegExi [] []
    QuantPS4 -> Uni [Restriction "person"][]
    QuantPS5 -> Exi [Restriction "person"][]
    QuantPS6 -> NegExi [Restriction "person"][]
    QuantPS7 -> Uni [Restriction "person"][]
    QuantPS8 -> Exi [Restriction "person"][]
    QuantPS9 -> NegExi [Restriction "person"][]
    QuantPSUni cn -> Uni [Restriction (fromcn cn)][]
    QuantPSExi cn -> Exi [Restriction (fromcn cn)][]
    QuantPSNeg cn -> NegExi [Restriction (fromcn cn)][]
    QuantSpec1 no -> Uni [Restriction "person"][Exclusion (fromnoun no)]
    QuantSpec2 no -> Uni [Restriction "person"][Exclusion (fromnoun no)]
    QuantSpec3 no -> Uni [][Exclusion (fromnoun no)]
    QuantSpec4 no -> Uni [Restriction "person"][Exclusion (fromnoun no)]
    QuantSpec5 no -> Uni [Restriction "person"][Exclusion (fromnoun no)]
    QuantSpec6 no -> Exi [][Exclusion (fromnoun no)]
    QuantSpec7 no -> Exi [Restriction "person"][Exclusion (fromnoun no)]
    QuantSpec8 no -> Exi [Restriction "person"][Exclusion (fromnoun no)]
    QuantSpec9 no -> Exi [][Exclusion (fromnoun no)]
    QuantSpec10 cn no -> Exi [Restriction (fromcn cn)][Exclusion (fromnoun no)]
    QuantSpec11 cn no -> Uni [Restriction (fromcn cn)][Exclusion (fromnoun no)]

fromcn :: CN -> String
fromcn (CN st) = st

-- | get singular numerical quantifer
getnumquantsS1 :: NQuantPS -> Quants
getnumquantsS1 nq = case nq of
  NQuantPS10 cn -> Exi [Restriction (fromcn cn)] []
  NQuantPS11 cn -> AtMost1 [Restriction (fromcn cn)] []
  NQuantPS12 cn -> Exactly1 [Restriction (fromcn cn)][]


-- | build an `ArgElement` from an argtype (Subject | Object) and a plural noun phrase
npp :: ArgType -> NounPhraseP -> ArgElement
npp arg np = case np of
  NPPConj1 no no' -> ArgElement arg (Right (NPAnd (Const $ fromnoun no)(Const $ fromnoun no')))
  NPPConj2 no no' -> ArgElement arg (Right (NPAnd (Const $ fromnoun no)(Const $ fromnoun no')))
  NPPQuan1 qp -> ArgElement arg (Left (getquantP1 qp))
  NPPQuan2 nqp -> ArgElement arg (Left (getnumquantP1 nqp))

getquantP1 :: QuantPP -> Quants
getquantP1 qp = case qp of
  QuantPP1 cn -> Uni [Restriction (fromcns cn)][]
  QuantPP2 cn -> Exi [Restriction (fromcns cn)][]
  QuantPP3 cn -> NegExi [Restriction (fromcns cn)][]

getnumquantP1 :: NQuantPP -> Quants
getnumquantP1 qp = case qp of
  NQuantPP1 cns -> AtLeast2 [Restriction (fromcns cns)] []
  NQuantPP2 cns -> AtLeast3 [Restriction (fromcns cns)] []
  NQuantPP3 cns -> AtMost2 [Restriction (fromcns cns)] []
  NQuantPP5 cns -> Exactly2 [Restriction (fromcns cns)] []

fromcns :: CNs -> String
fromcns (CNs st) = st

-- | verb phrase stuff

-- | verb phrases need a return type. they need to provide a range of information. 
-- | they need to provide information as to whether the predicate type is singular, 
-- | conjunctive, disjunctive, or negated disjunction. it also needs to provide 
-- | information about whether its object is singular, conjunctive, disjunctive, 
-- | or negated disjunction.

data PredElement = PredElement Valance PredType (Maybe ArgElement)

data PredType = VPSingle Pred
              | VPOr Pred Pred
              | VPAnd Pred Pred
              | VPNegOr Pred Pred

-- | singular and plural verb phrases. these are a bit redundant.

vps :: VerbPhraseS -> PredElement
vps v = case v of
  VPS1 vp -> case vp of
    VP1 vx -> PredElement Pos (VPSingle $ predfromVerbX vx) (argfromVerbX vx) -- e.g. "is jumping"
    VPConj1 vx vx' -> PredElement Pos (VPAnd (predfromVerbX vx) (predfromVerbX vx')) Nothing -- e.g. "is running and jumping"
    VPConj2 vx vx' -> PredElement Pos (VPAnd (predfromVerbX vx) (predfromVerbX vx')) Nothing -- e.g. "is both running and jumping"
    VPDisj1 vx vx' -> PredElement Pos (VPOr (predfromVerbX vx) (predfromVerbX vx')) Nothing -- e.g. "is running or jumping"
    VPDisj2 vx vx' -> PredElement Pos (VPOr (predfromVerbX vx) (predfromVerbX vx')) Nothing -- e.g. "is either running or jumping"
    VPNegDisj1 vx vx' -> PredElement Pos (VPNegOr (predfromVerbX vx) (predfromVerbX vx')) Nothing --e.g. "is neither running nor jumping"
  VPS2 ap -> case ap of
    AdjP1 adj -> PredElement Pos (VPSingle $ fromAdj adj) Nothing --e.g. "is sad"
    AdjPConj1 adj adj' -> PredElement Pos (VPAnd (fromAdj adj)(fromAdj adj')) Nothing -- e.g. "is happy and sad"
    AdjPConj2 adj adj' -> PredElement Pos (VPAnd (fromAdj adj)(fromAdj adj')) Nothing -- e.g. "is both happy and sad"
    AdjPDisj1 adj adj' -> PredElement Pos (VPOr (fromAdj adj)(fromAdj adj')) Nothing -- e.g. "is happy or sad"
    AdjPDisj2 adj adj' -> PredElement Pos (VPOr (fromAdj adj)(fromAdj adj')) Nothing -- e.g. "is either happy or sad"
    AdjPNegDisj1 adj adj' -> PredElement Pos (VPNegOr (fromAdj adj)(fromAdj adj')) Nothing -- e.g. "is neither happy nor sad"
  VPS3 vp -> case vp of
    VP1 vx -> PredElement Pos (VPSingle $ predfromVerbX vx) (argfromVerbX vx) -- e.g. "is jumping"
    VPConj1 vx vx' -> PredElement Pos (VPAnd (predfromVerbX vx) (predfromVerbX vx')) Nothing -- e.g. "is running and jumping"
    VPConj2 vx vx' -> PredElement Pos (VPAnd (predfromVerbX vx) (predfromVerbX vx')) Nothing -- e.g. "is both running and jumping"
    VPDisj1 vx vx' -> PredElement Pos (VPOr (predfromVerbX vx) (predfromVerbX vx')) Nothing -- e.g. "is running or jumping"
    VPDisj2 vx vx' -> PredElement Pos (VPOr (predfromVerbX vx) (predfromVerbX vx')) Nothing -- e.g. "is either running or jumping"
    VPNegDisj1 vx vx' -> PredElement Pos (VPNegOr (predfromVerbX vx) (predfromVerbX vx')) Nothing --e.g. "is neither running nor jumping"
  VPS1Neg vp -> case vp of
    VP1 vx -> PredElement Neg (VPSingle $ predfromVerbX vx) (argfromVerbX vx) -- e.g. "is not jumping"
    VPConj1 vx vx' -> PredElement Neg (VPAnd (predfromVerbX vx) (predfromVerbX vx')) Nothing -- e.g. "is not running and jumping"
    VPConj2 vx vx' -> PredElement Neg (VPAnd (predfromVerbX vx) (predfromVerbX vx')) Nothing -- e.g. "is not both running and jumping"
    VPDisj1 vx vx' -> PredElement Neg (VPOr (predfromVerbX vx) (predfromVerbX vx')) Nothing -- e.g. "is not running or jumping"
    VPDisj2 vx vx' -> PredElement Neg (VPOr (predfromVerbX vx) (predfromVerbX vx')) Nothing -- e.g. "is not either running or jumping"
    VPNegDisj1 vx vx' -> PredElement Neg (VPNegOr (predfromVerbX vx) (predfromVerbX vx')) Nothing --e.g. "is not neither running nor jumping"
  VPS2Neg ap -> case ap of
    AdjP1 adj -> PredElement Neg (VPSingle $ fromAdj adj) Nothing
    AdjPConj1 adj adj' -> PredElement Neg (VPAnd (fromAdj adj)(fromAdj adj')) Nothing
    AdjPConj2 adj adj' -> PredElement Neg (VPAnd (fromAdj adj)(fromAdj adj')) Nothing
    AdjPDisj1 adj adj' -> PredElement Neg (VPOr (fromAdj adj)(fromAdj adj')) Nothing
    AdjPDisj2 adj adj' -> PredElement Neg (VPOr (fromAdj adj)(fromAdj adj')) Nothing
    AdjPNegDisj1 adj adj' -> PredElement Neg (VPNegOr (fromAdj adj)(fromAdj adj')) Nothing
  VPS1NegC vp -> case vp of
    VP1 vx -> PredElement Neg (VPSingle $ predfromVerbX vx) (argfromVerbX vx)
    VPConj1 vx vx' -> PredElement Neg (VPAnd (predfromVerbX vx) (predfromVerbX vx')) Nothing
    VPConj2 vx vx' -> PredElement Neg (VPAnd (predfromVerbX vx) (predfromVerbX vx')) Nothing
    VPDisj1 vx vx' -> PredElement Neg (VPOr (predfromVerbX vx) (predfromVerbX vx')) Nothing
    VPDisj2 vx vx' -> PredElement Neg (VPOr (predfromVerbX vx) (predfromVerbX vx')) Nothing
    VPNegDisj1 vx vx' -> PredElement Neg (VPNegOr (predfromVerbX vx) (predfromVerbX vx')) Nothing
  VPS2NegC ap -> case ap of
    AdjP1 adj -> PredElement Neg (VPSingle $ fromAdj adj) Nothing
    AdjPConj1 adj adj' -> PredElement Neg (VPAnd (fromAdj adj)(fromAdj adj')) Nothing
    AdjPConj2 adj adj' -> PredElement Neg (VPAnd (fromAdj adj)(fromAdj adj')) Nothing
    AdjPDisj1 adj adj' -> PredElement Neg (VPOr (fromAdj adj)(fromAdj adj')) Nothing
    AdjPDisj2 adj adj' -> PredElement Neg (VPOr (fromAdj adj)(fromAdj adj')) Nothing
    AdjPNegDisj1 adj adj' -> PredElement Neg (VPNegOr (fromAdj adj)(fromAdj adj')) Nothing

vpp :: VerbPhraseP -> PredElement
vpp v = case v of
  VPP1 vp -> case vp of
    VP1 vx -> PredElement Pos (VPSingle $ predfromVerbX vx) (argfromVerbX vx)
    VPConj1 vx vx' -> PredElement Pos (VPAnd (predfromVerbX vx) (predfromVerbX vx')) Nothing
    VPConj2 vx vx' -> PredElement Pos (VPAnd (predfromVerbX vx) (predfromVerbX vx')) Nothing
    VPDisj1 vx vx' -> PredElement Pos (VPOr (predfromVerbX vx) (predfromVerbX vx')) Nothing
    VPDisj2 vx vx' -> PredElement Pos (VPOr (predfromVerbX vx) (predfromVerbX vx')) Nothing
    VPNegDisj1 vx vx' -> PredElement Pos (VPNegOr (predfromVerbX vx) (predfromVerbX vx')) Nothing
  VPP2 ap -> case ap of
    AdjP1 adj -> PredElement Pos (VPSingle $ fromAdj adj) Nothing
    AdjPConj1 adj adj' -> PredElement Pos (VPAnd (fromAdj adj)(fromAdj adj')) Nothing
    AdjPConj2 adj adj' -> PredElement Pos (VPAnd (fromAdj adj)(fromAdj adj')) Nothing
    AdjPDisj1 adj adj' -> PredElement Pos (VPOr (fromAdj adj)(fromAdj adj')) Nothing
    AdjPDisj2 adj adj' -> PredElement Pos (VPOr (fromAdj adj)(fromAdj adj')) Nothing
    AdjPNegDisj1 adj adj' -> PredElement Pos (VPNegOr (fromAdj adj)(fromAdj adj')) Nothing
  VPP3 vp -> case vp of
    VP1 vx -> PredElement Pos (VPSingle $ predfromVerbX vx) (argfromVerbX vx)
    VPConj1 vx vx' -> PredElement Pos (VPAnd (predfromVerbX vx) (predfromVerbX vx')) Nothing
    VPConj2 vx vx' -> PredElement Pos (VPAnd (predfromVerbX vx) (predfromVerbX vx')) Nothing
    VPDisj1 vx vx' -> PredElement Pos (VPOr (predfromVerbX vx) (predfromVerbX vx')) Nothing
    VPDisj2 vx vx' -> PredElement Pos (VPOr (predfromVerbX vx) (predfromVerbX vx')) Nothing
    VPNegDisj1 vx vx' -> PredElement Pos (VPNegOr (predfromVerbX vx) (predfromVerbX vx')) Nothing
  VPP1Neg vp -> case vp of
    VP1 vx -> PredElement Neg (VPSingle $ predfromVerbX vx) (argfromVerbX vx)
    VPConj1 vx vx' -> PredElement Neg (VPAnd (predfromVerbX vx) (predfromVerbX vx')) Nothing
    VPConj2 vx vx' -> PredElement Neg (VPAnd (predfromVerbX vx) (predfromVerbX vx')) Nothing
    VPDisj1 vx vx' -> PredElement Neg (VPOr (predfromVerbX vx) (predfromVerbX vx')) Nothing
    VPDisj2 vx vx' -> PredElement Neg (VPOr (predfromVerbX vx) (predfromVerbX vx')) Nothing
    VPNegDisj1 vx vx' -> PredElement Neg (VPNegOr (predfromVerbX vx) (predfromVerbX vx')) Nothing
  VPP2Neg ap -> case ap of
    AdjP1 adj -> PredElement Neg (VPSingle $ fromAdj adj) Nothing
    AdjPConj1 adj adj' -> PredElement Neg (VPAnd (fromAdj adj)(fromAdj adj')) Nothing
    AdjPConj2 adj adj' -> PredElement Neg (VPAnd (fromAdj adj)(fromAdj adj')) Nothing
    AdjPDisj1 adj adj' -> PredElement Neg (VPOr (fromAdj adj)(fromAdj adj')) Nothing
    AdjPDisj2 adj adj' -> PredElement Neg (VPOr (fromAdj adj)(fromAdj adj')) Nothing
    AdjPNegDisj1 adj adj' -> PredElement Neg (VPNegOr (fromAdj adj)(fromAdj adj')) Nothing
  VPP1NegC vp -> case vp of
    VP1 vx -> PredElement Neg (VPSingle $ predfromVerbX vx) (argfromVerbX vx)
    VPConj1 vx vx' -> PredElement Neg (VPAnd (predfromVerbX vx) (predfromVerbX vx')) Nothing
    VPConj2 vx vx' -> PredElement Neg (VPAnd (predfromVerbX vx) (predfromVerbX vx')) Nothing
    VPDisj1 vx vx' -> PredElement Neg (VPOr (predfromVerbX vx) (predfromVerbX vx')) Nothing
    VPDisj2 vx vx' -> PredElement Neg (VPOr (predfromVerbX vx) (predfromVerbX vx')) Nothing
    VPNegDisj1 vx vx' -> PredElement Neg (VPNegOr (predfromVerbX vx) (predfromVerbX vx')) Nothing
  VPP2NegC ap -> case ap of
    AdjP1 adj -> PredElement Neg (VPSingle $ fromAdj adj) Nothing
    AdjPConj1 adj adj' -> PredElement Neg (VPAnd (fromAdj adj)(fromAdj adj')) Nothing
    AdjPConj2 adj adj' -> PredElement Neg (VPAnd (fromAdj adj)(fromAdj adj')) Nothing
    AdjPDisj1 adj adj' -> PredElement Neg (VPOr (fromAdj adj)(fromAdj adj')) Nothing
    AdjPDisj2 adj adj' -> PredElement Neg (VPOr (fromAdj adj)(fromAdj adj')) Nothing
    AdjPNegDisj1 adj adj' -> PredElement Neg (VPNegOr (fromAdj adj)(fromAdj adj')) Nothing

predfromVerbX :: VerbX -> Pred
predfromVerbX v = case v of
  VerbX1 iv -> fromIVerb iv
  VerbX2 tv npp -> fromTVerb tv
  VerbX3 tv nps -> fromTVerb tv
  VerbXR1 tv -> fromTVerb tv
  VerbXR2 tv -> fromTVerb tv
  VerbxR3 tv -> fromTVerb tv

argfromVerbX :: VerbX -> Maybe ArgElement
argfromVerbX v = case v of
  VerbX1 iv -> Nothing
  VerbX2 tv n -> Just $ npp Object n
  VerbX3 tv n -> Just $ nps Object n
  VerbXR1 tv -> Just $ ArgElement Object (Right (NPSingle (Var 'x')))
  VerbXR2 tv -> Just $ ArgElement Object (Right (NPSingle (Var 'x')))
  VerbxR3 tv -> Just $ ArgElement Object (Right (NPSingle (Var 'x')))

fromAdj :: Adj -> Pred
fromAdj (Adj st) = Pred st

fromIVerb :: IVerb -> Pred
fromIVerb (IVerb st) = Pred st

fromTVerb :: TVerb -> Pred
fromTVerb (TVerb st) = Pred st

-- | build the clause from its parts (using the either monad)

-- | unify representation of clauses whether plural or not
buildLClause'S :: NounPhraseS -> VerbPhraseS -> LClause'
buildLClause'S n v = LClause' (nps Subject n) (vps v) 

buildLClause'P :: NounPhraseP -> VerbPhraseP -> LClause'
buildLClause'P n v = LClause' (npp Subject n) (vpp v)

-- | the main clause building function. the cases described below
buildClause :: LClause' -> Either String LClause
buildClause (LClause' (ArgElement _ e) pe) = case e of
  Left qu -> buildfromSubjQuant qu pe -- a quantifier qu in subject position 
  Right na -> case na of
    NPSingle trm -> buildfromSubArg trm pe -- basic noun phrase case
    NPOr trm trm' -> buildfromNPOr trm trm' pe -- disjunctive np case
    NPAnd trm trm' -> buildfromNPAnd trm trm' pe -- conjunctive np case
    NPNegOr trm trm' -> buildfromNPOr trm trm' pe -- negative disjunctive np case

-- why split the cases like this? because things like conjunctive noun phrases
-- are translated into two clauses conjoined

-- e.g. something like 'some person' in the subject position
-- note that LClauseDisjunction' etc. translate into things like @x(Fx v Gx)
buildfromSubjQuant :: Quants -> PredElement -> Either String LClause
buildfromSubjQuant qu (PredElement val pt mae) = case mae of -- no object case
  Nothing -> case pt of
    VPSingle pr -> Right $ LClause val [SubQuant qu] pr [Var 'x'] -- e.g. "some person is / is not running"
    VPOr pr pr' -> Right $ LClauseDisjunction' val [SubQuant qu] (pr,pr') [Var 'x'] -- e.g. "some person is / is not running or jumping"
    VPAnd pr pr' -> Right $ LClauseConjunction' val [SubQuant qu] (pr,pr') [Var 'x'] -- e.g. "some person is / is not running and jumping"
    VPNegOr pr pr' -> Right $ LClauseDisjunction' Neg [SubQuant qu] (pr,pr') [Var 'x'] -- e.g. "some person is / is not neither running nor jumping"
  Just ae -> case ae of -- object case
    ArgElement at e -> case e of
      Left qu' -> case pt of
        VPSingle pr -> Right $ LClause val [SubQuant qu,ObjQuant qu'] pr [Var 'x',Var 'y']
        VPOr pr pr' -> Left "I can't translate that"
        VPAnd pr pr' -> Left "I can't translate that"
        VPNegOr pr pr' -> Left "I can't translate that"
      Right na -> case na of
        NPSingle trm -> case pt of
          VPSingle pr -> Right $ LClause val [SubQuant qu] pr [Var 'x',trm]
          VPOr pr pr' -> Left "I can't translate that"
          VPAnd pr pr' -> Left "I can't translate that"
          VPNegOr pr pr' -> Left "I can't translate that"
        NPOr trm trm' -> case pt of
          VPSingle pr -> Right $ LClauseDisjunction Pos (LClause val [SubQuant qu] pr [Var 'x',trm])(LClause val [SubQuant qu] pr [Var 'x',trm'])
          VPOr pr pr' -> Left "I can't translate that"
          VPAnd pr pr' -> Left "I can't translate that"
          VPNegOr pr pr' -> Left "I can't translate that"
        NPAnd trm trm' -> case pt of
          VPSingle pr -> Right $ LClauseConjunction Pos (LClause val [SubQuant qu] pr [Var 'x',trm])(LClause val [SubQuant qu] pr [Var 'x',trm'])
          VPOr pr pr' -> Left "I can't translate that"
          VPAnd pr pr' -> Left "I can't translate that"
          VPNegOr pr pr' -> Left "I can't translate that"
        NPNegOr trm trm' -> case pt of
          VPSingle pr -> Right $ LClauseDisjunction Neg (LClause val [SubQuant qu] pr [Var 'x',trm])(LClause val [SubQuant qu] pr [Var 'x',trm'])
          VPOr pr pr' -> Left "I can't translate that"
          VPAnd pr pr' -> Left "I can't translate that"
          VPNegOr pr pr' -> Left "I can't translate that"

buildfromSubArg :: Trm -> PredElement -> Either String LClause
buildfromSubArg tr (PredElement val pt mae) = case mae of
  Nothing -> case pt of
    VPSingle pr -> Right $ LClause val [] pr [tr]
    VPOr pr pr' -> Right $ LClauseDisjunction Pos (LClause val [] pr [tr]) (LClause val [] pr' [tr])
    VPAnd pr pr' -> Right $ LClauseConjunction Pos (LClause val [] pr [tr]) (LClause val [] pr' [tr])
    VPNegOr pr pr' -> Right $ LClauseDisjunction Neg (LClause val [] pr [tr]) (LClause val [] pr' [tr])
  Just ae -> case ae of
    ArgElement at e -> case e of
      Left qu' -> case pt of
        VPSingle pr -> Right $ LClause val [ObjQuant qu'] pr [tr,Var 'y']
        VPOr pr pr' -> Left "I can't translate that"
        VPAnd pr pr' -> Left "I can't translate that"
        VPNegOr pr pr' -> Left "I can't translate that"
      Right na -> case na of
        NPSingle trm -> case pt of
          VPSingle pr -> Right $ LClause val [] pr [tr,trm]
          VPOr pr pr' -> Left "I can't translate that"
          VPAnd pr pr' -> Left "I can't translate that"
          VPNegOr pr pr' -> Left "I can't translate that"
        NPOr trm trm' -> case pt of
          VPSingle pr -> Right $ LClauseDisjunction Pos (LClause val [] pr [tr,trm])(LClause val [] pr [tr,trm'])
          VPOr pr pr' -> Left "I can't translate that"
          VPAnd pr pr' -> Left "I can't translate that"
          VPNegOr pr pr' -> Left "I can't translate that"
        NPAnd trm trm' -> case pt of
          VPSingle pr -> Right $ LClauseConjunction Pos (LClause val [] pr [tr,trm])(LClause val [] pr [tr,trm'])
          VPOr pr pr' -> Left "I can't translate that"
          VPAnd pr pr' -> Left "I can't translate that"
          VPNegOr pr pr' -> Left "I can't translate that"
        NPNegOr trm trm' -> case pt of
          VPSingle pr -> Right $ LClauseDisjunction Neg (LClause val [] pr [tr,trm])(LClause val [] pr [tr,trm'])
          VPOr pr pr' -> Left "I can't translate that"
          VPAnd pr pr' -> Left "I can't translate that"
          VPNegOr pr pr' -> Left "I can't translate that"

buildfromNPOr :: Trm -> Trm -> PredElement -> Either String LClause
buildfromNPOr st1 st2 (PredElement val pt mae) = case mae of
  Nothing -> case pt of
    VPSingle pr -> Right $ LClauseDisjunction Pos (LClause val [] pr [st1]) (LClause val [] pr [st2])
    VPOr pr pr' -> Left "I can't translate that"
    VPAnd pr pr' -> Left "I can't translate that"
    VPNegOr pr pr' -> Left "I can't translate that"
  Just ae -> case ae of
    ArgElement at e -> case e of
      Left qu -> case pt of
        VPSingle pr -> Left "I can't translate that"
        VPOr pr pr' -> Left "I can't translate that"
        VPAnd pr pr' -> Left "I can't translate that"
        VPNegOr pr pr' -> Left "I can't translate that"
      Right na -> case na of
        NPSingle trm -> case pt of
          VPSingle pr -> Right $ LClauseDisjunction Pos (LClause val [] pr [st1,trm]) (LClause val [] pr [st2,trm])
          VPOr pr pr' -> Left "I can't translate that"
          VPAnd pr pr' -> Left "I can't translate that"
          VPNegOr pr pr' -> Left "I can't translate that"
        NPOr trm trm' -> case pt of
          VPSingle pr -> Right $ LClauseDisjunction Pos (LClauseDisjunction Pos (LClause val [] pr [st1,trm]) (LClause val [] pr [st2,trm])) (LClauseDisjunction Pos (LClause val [] pr [st1,trm']) (LClause val [] pr [st2,trm']))
          VPOr pr pr' -> Left "I can't translate that"
          VPAnd pr pr' -> Left "I can't translate that"
          VPNegOr pr pr' -> Left "I can't translate that"
        NPAnd trm trm' -> case pt of
          VPSingle pr -> Right $ LClauseDisjunction Pos (LClauseConjunction Pos (LClause val [] pr [st1,trm]) (LClause val [] pr [st2,trm])) (LClauseConjunction Pos (LClause val [] pr [st1,trm']) (LClause val [] pr [st2,trm']))
          VPOr pr pr' -> Left "I can't translate that"
          VPAnd pr pr' -> Left "I can't translate that"
          VPNegOr pr pr' -> Left "I can't translate that"
        NPNegOr trm trm' -> case pt of
          VPSingle pr -> Right $ LClauseDisjunction Pos (LClauseDisjunction Neg (LClause val [] pr [st1,trm]) (LClause val [] pr [st2,trm])) (LClauseDisjunction Neg (LClause val [] pr [st1,trm']) (LClause val [] pr [st2,trm']))
          VPOr pr pr' -> Left "I can't translate that"
          VPAnd pr pr' -> Left "I can't translate that"
          VPNegOr pr pr' -> Left "I can't translate that"

buildfromNPAnd :: Trm -> Trm -> PredElement -> Either String LClause
buildfromNPAnd st1 st2 (PredElement val pt mae) = case mae of
  Nothing -> case pt of
    VPSingle pr -> Right $ LClauseConjunction Pos (LClause val [] pr [st1]) (LClause val [] pr [st2])
    VPOr pr pr' -> Right $ LClauseConjunction Pos (LClauseDisjunction Pos (LClause val [] pr [st1]) (LClause val [] pr' [st1])) (LClauseDisjunction Pos (LClause val [] pr [st2]) (LClause val [] pr' [st2]))
    VPAnd pr pr' -> Right $ LClauseConjunction Pos (LClauseConjunction Pos (LClause val [] pr [st1]) (LClause val [] pr' [st1])) (LClauseConjunction Pos (LClause val [] pr [st2]) (LClause val [] pr' [st2]))
    VPNegOr pr pr' -> Right $ LClauseConjunction Pos (LClauseDisjunction Neg (LClause val [] pr [st1]) (LClause val [] pr' [st1])) (LClauseDisjunction Neg (LClause val [] pr [st2]) (LClause val [] pr' [st2]))
  Just ae -> case ae of
    ArgElement at e -> case e of
      Left qu -> case pt of
        VPSingle pr -> Left "I can't translate that"
        VPOr pr pr' -> Left "I can't translate that"
        VPAnd pr pr' -> Left "I can't translate that"
        VPNegOr pr pr' -> Left "I can't translate that"
      Right na -> case na of
        NPSingle trm -> case pt of
          VPSingle pr -> Right $ LClauseConjunction Pos (LClause val [] pr [st1,trm]) (LClause val [] pr [st2,trm])
          VPOr pr pr' -> Left "I can't translate that"
          VPAnd pr pr' -> Left "I can't translate that"
          VPNegOr pr pr' -> Left "I can't translate that"
        NPOr trm trm' -> case pt of
          VPSingle pr -> Right $ LClauseConjunction Pos (LClauseDisjunction Pos (LClause val [] pr [st1,trm]) (LClause val [] pr [st2,trm])) (LClauseDisjunction Pos (LClause val [] pr [st1,trm']) (LClause val [] pr [st2,trm']))
          VPOr pr pr' -> Left "I can't translate that"
          VPAnd pr pr' -> Left "I can't translate that"
          VPNegOr pr pr' -> Left "I can't translate that"
        NPAnd trm trm' -> case pt of
          VPSingle pr -> Right $ LClauseConjunction Pos (LClauseConjunction Pos (LClause val [] pr [st1,trm]) (LClause val [] pr [st2,trm])) (LClauseConjunction Pos (LClause val [] pr [st1,trm']) (LClause val [] pr [st2,trm']))
          VPOr pr pr' -> Left "I can't translate that"
          VPAnd pr pr' -> Left "I can't translate that"
          VPNegOr pr pr' -> Left "I can't translate that"
        NPNegOr trm trm' -> case pt of
          VPSingle pr -> Right $ LClauseConjunction Pos (LClauseDisjunction Neg (LClause val [] pr [st1,trm]) (LClause val [] pr [st2,trm])) (LClauseDisjunction Neg (LClause val [] pr [st1,trm']) (LClause val [] pr [st2,trm']))
          VPOr pr pr' -> Left "I can't translate that"
          VPAnd pr pr' -> Left "I can't translate that"
          VPNegOr pr pr' -> Left "I can't translate that"

buildfromNPNotOr :: Trm -> Trm -> PredElement -> Either String LClause
buildfromNPNotOr st1 st2 (PredElement val pt mae) = case mae of
  Nothing -> case pt of
    VPSingle pr -> Right $ LClauseDisjunction Neg (LClause val [] pr [st1]) (LClause val [] pr [st2])
    VPOr pr pr' -> Left "I can't translate that"
    VPAnd pr pr' -> Left "I can't translate that"
    VPNegOr pr pr' -> Left "I can't translate that"
  Just ae -> case ae of
    ArgElement at e -> case e of
      Left qu -> case pt of
        VPSingle pr -> Left "I can't translate that"
        VPOr pr pr' -> Left "I can't translate that"
        VPAnd pr pr' -> Left "I can't translate that"
        VPNegOr pr pr' -> Left "I can't translate that"
      Right na -> case na of
        NPSingle trm -> case pt of
          VPSingle pr -> Right $ LClauseDisjunction Neg (LClause val [] pr [st1,trm]) (LClause val [] pr [st2,trm])
          VPOr pr pr' -> Left "I can't translate that"
          VPAnd pr pr' -> Left "I can't translate that"
          VPNegOr pr pr' -> Left "I can't translate that"
        NPOr trm trm' -> case pt of
          VPSingle pr -> Right $ LClauseDisjunction Neg (LClauseDisjunction Pos (LClause val [] pr [st1,trm]) (LClause val [] pr [st2,trm])) (LClauseDisjunction Pos (LClause val [] pr [st1,trm']) (LClause val [] pr [st2,trm']))
          VPOr pr pr' -> Left "I can't translate that"
          VPAnd pr pr' -> Left "I can't translate that"
          VPNegOr pr pr' -> Left "I can't translate that"
        NPAnd trm trm' -> case pt of
          VPSingle pr -> Right $ LClauseDisjunction Neg (LClauseConjunction Pos (LClause val [] pr [st1,trm]) (LClause val [] pr [st2,trm])) (LClauseConjunction Pos (LClause val [] pr [st1,trm']) (LClause val [] pr [st2,trm']))
          VPOr pr pr' -> Left "I can't translate that"
          VPAnd pr pr' -> Left "I can't translate that"
          VPNegOr pr pr' -> Left "I can't translate that"
        NPNegOr trm trm' -> case pt of
          VPSingle pr -> Right $ LClauseDisjunction Pos (LClauseDisjunction Neg (LClause val [] pr [st1,trm]) (LClause val [] pr [st2,trm])) (LClauseDisjunction Neg (LClause val [] pr [st1,trm']) (LClause val [] pr [st2,trm']))
          VPOr pr pr' -> Left "I can't translate that"
          VPAnd pr pr' -> Left "I can't translate that"
          VPNegOr pr pr' -> Left "I can't translate that"

extractPredicates :: PredElement -> PredType
extractPredicates (PredElement _ p _) = p

extractValence :: PredElement -> Valance
extractValence (PredElement v _ _) = v

extractQuantP :: PredElement -> [PosQuant]
extractQuantP (PredElement x y z) = case z of
  Nothing -> []
  Just ae -> extractQuant Object ae

extractQuant :: ArgType -> ArgElement -> [PosQuant]
extractQuant arg (ArgElement a e) = case e of
  Left qu -> case arg of
    Subject -> [SubQuant qu]
    Object -> [ObjQuant qu]
  Right na -> []


extractArgTypeP :: PredElement -> Maybe ArgElement
extractArgTypeP (PredElement x y z) = z

-- | final function for building clase

buildLClause :: Clause -> Either String LClause
buildLClause c = case c of
  ClauseS nps vps -> buildClause $  buildLClause'S nps vps
  ClauseP npp vpp -> buildClause $ buildLClause'P npp vpp
  Cond1 cl cl' -> do
    l <- buildLClause cl
    r <- buildLClause cl'
    return (LClauseConditional l r)
  Cond2 cl cl' -> do
    l <- buildLClause cl
    r <- buildLClause cl'
    return (LClauseConditional l r)
  Cond3 cl cl' -> do
    l <- buildLClause cl
    r <- buildLClause cl'
    return (LClauseConditional l r)
  Cond4 cl cl' -> do
    l <- buildLClause cl'
    r <- buildLClause cl
    return (LClauseConditional l r)
  BCon1 cl cl' -> do
    l <- buildLClause cl
    r <- buildLClause cl'
    return (LClauseBiConditional l r)
  Conj1 cl cl' -> do
    l <- buildLClause cl
    r <- buildLClause cl'
    return (LClauseConjunction Pos l r)
  Conj2 cl cl' -> do
    l <- buildLClause cl
    r <- buildLClause cl'
    return (LClauseConjunction Pos l r)
  Disj1 cl cl' -> do
    l <- buildLClause cl
    r <- buildLClause cl'
    return (LClauseDisjunction Pos l r)
  Disj2 cl cl' -> do
    l <- buildLClause cl
    r <- buildLClause cl'
    return (LClauseDisjunction Pos l r)
  CNeg1 cl -> do
    l <- buildLClause cl
    return (LClauseNegation l)
  CNeg2 cl -> do
    l <- buildLClause cl
    return (LClauseNegation l)
  CNeg3 cl -> do
    l <- buildLClause cl
    return (LClauseNegation l )
  Spec1 cl cl' -> do
    l <- buildLClause cl'
    r <- buildLClause cl
    return (LClauseConditional (LClauseNegation l) r)
  Spec2 cl cl' -> do
    l <- buildLClause cl'
    r <- buildLClause cl
    return (LClauseConditional l r)
  Spec3 cl cl' -> do
    l <- buildLClause cl'
    r <- buildLClause cl
    return (LClauseConditional l r)
  Spec4 cl cl' -> do
    l <- buildLClause cl'
    r <- buildLClause cl
    return (LClauseConjunction Pos l r)
  Spec5 cl cl' -> do
    l <- buildLClause cl
    r <- buildLClause cl'
    return (LClauseConditional (LClauseNegation l) r)
  Spec6 cl cl' -> do
    l <- buildLClause cl
    r <- buildLClause cl'
    return (LClauseConditional l r)
  Spec7 cl cl' -> do
    l <- buildLClause cl
    r <- buildLClause cl'
    return (LClauseConditional l r)
  Spec8 cl cl' -> do
    l <- buildLClause cl'
    r <- buildLClause cl
    return (LClauseConjunction Pos l r)
  Spec9 cl ic -> do
    l <- buildLClause cl
    r <- buildINVClause ic
    return (LClauseBiConditional r l)
  Spec10 cl ic -> do
    l <- buildLClause cl
    r <- buildINVClause ic
    return (LClauseConditional r l)
  ExeS nqp -> buildClauseExistsS nqp
  ExeP nqp -> buildClauseExistsP nqp

buildClauseExistsS :: NQuantPS -> Either String LClause
buildClauseExistsS npq = Right $ LClauseExists $ getnumquantsS1 npq

buildClauseExistsP :: NQuantPP -> Either String LClause
buildClauseExistsP npq = Right $ LClauseExists $ getnumquantP1 npq

buildINVClause :: InvClause -> Either String LClause
buildINVClause ic = case ic of
  InvClause1 nps vp -> buildClause $  buildLClause'S nps (fakeVPS vp)
  InvClause2 npp vp -> buildClause $  buildLClause'P npp (fakeVPP vp)

fakeVPS :: VerbPhrase -> VerbPhraseS
fakeVPS vp = case vp of
  VP1 vx -> VPS1 (VP1 vx)
  VPConj1 vx vx' -> VPS1 (VPConj1 vx vx')
  VPConj2 vx vx' -> VPS1 (VPConj2 vx vx')
  VPDisj1 vx vx' -> VPS1 (VPDisj1 vx vx')
  VPDisj2 vx vx' -> VPS1 (VPDisj2 vx vx')
  VPNegDisj1 vx vx' -> VPS1 (VPNegDisj1 vx vx' )

fakeVPP :: VerbPhrase -> VerbPhraseP
fakeVPP vp = case vp of
  VP1 vx -> VPP1 (VP1 vx)
  VPConj1 vx vx' -> VPP1 (VPConj1 vx vx')
  VPConj2 vx vx' -> VPP1 (VPConj2 vx vx')
  VPDisj1 vx vx' -> VPP1 (VPDisj1 vx vx')
  VPDisj2 vx vx' -> VPP1 (VPDisj2 vx vx')
  VPNegDisj1 vx vx' -> VPP1 (VPNegDisj1 vx vx' )

-- | CONVERT INTERMEDIATE FORM TO FINAL FORM

tClause :: LClause -> Either String [Prop]
tClause cl = case cl of
  LClause va pqs pr trms -> case va of
    Pos -> addQuants pqs $ Atomic (Predicate (fromverbgloss (fromPred pr))) (fromTerms trms)
    Neg -> addQuants pqs $ Negation (Atomic (Predicate (fromverbgloss (fromPred pr))) (fromTerms trms))
  LClauseConjunction va lc lc' -> case va of
    Pos -> do
      a <- tClause lc
      b <- tClause lc'
      return ([uncurry Conjunction (x, y) |
       x <- a, y <- b])
    Neg -> do
      a <- tClause lc
      b <- tClause lc'
      return ( [Negation $ uncurry Conjunction (x,y) |
       x <- a, y <- b])
  LClauseDisjunction va lc lc' -> case va of
    Pos -> do
      a <- tClause lc
      b <- tClause lc'
      return ([uncurry Disjunction (x,y) |
       x <- a, y <- b])
    Neg -> do
      a <- tClause lc
      b <- tClause lc'
      return ([Negation $ uncurry Disjunction (x,y) | x <- a, y <- b])
  LClauseConditional lc lc' -> do
      a <- tClause lc
      b <- tClause lc'
      return ([uncurry Conditional (x,y) |
       x <- a, y <- b])
  LClauseBiConditional lc lc' -> do
      a <- tClause lc
      b <- tClause lc'
      return ( [uncurry Biconditional (x,y) |
       x <- a, y <- b])
  LClauseNegation lc -> do
      a <- tClause lc
      return ([Negation x | x <- a])
  LClauseConjunction' va pqs (pr,pr') trms -> case va of
    Pos -> addQuants pqs $ Conjunction (Atomic (Predicate (fromverbgloss (fromPred pr))) (fromTerms trms)) (Atomic (Predicate (fromverbgloss (fromPred pr'))) (fromTerms trms))
    Neg -> addQuants pqs $ Negation $ Conjunction (Atomic (Predicate (fromverbgloss (fromPred pr))) (fromTerms trms)) (Atomic (Predicate (fromverbgloss (fromPred pr'))) (fromTerms trms))
  LClauseDisjunction' va pqs (pr,pr') trms -> case va of
    Pos -> addQuants pqs $ Disjunction (Atomic (Predicate (fromverbgloss (fromPred pr))) (fromTerms trms)) (Atomic (Predicate (fromverbgloss (fromPred pr'))) (fromTerms trms))
    Neg -> addQuants pqs $ Negation $ Disjunction (Atomic (Predicate (fromverbgloss (fromPred pr))) (fromTerms trms)) (Atomic (Predicate (fromverbgloss (fromPred pr'))) (fromTerms trms))
  LClauseExists q -> doQuant q

doQuant :: Quants -> Either String [Prop]
doQuant q = case q of
  Uni res exs -> Left "How did this quantifier get here?"
  Exi res exs -> Right [Existential 'x' (Atomic (Predicate (fromRestriction $ head res)) [Variable 'x'])]
  NegExi res exs -> Left "How did this quantifier get here?"
  AtLeast2 res exs -> Right [Existential 'x' (Existential 'y' (Conjunction (Negation (Atomic (Predicate 'I') [Variable 'x',Variable 'y'])) (Conjunction (Atomic (Predicate (fromRestriction $ head res)) [Variable 'x']) (Atomic (Predicate (fromRestriction $ head res)) [Variable 'y']) )))]
  AtLeast3 res exs -> Right [Existential 'x' (Existential 'y' (Existential 'z' (Conjunction (Conjunction (Conjunction (Negation (Atomic (Predicate 'I') [Variable 'x',Variable 'y'])) (Negation (Atomic (Predicate 'I') [Variable 'y',Variable 'z'])) )(Negation (Atomic (Predicate 'I') [Variable 'x',Variable 'z'])))  (Conjunction (Conjunction (Atomic (Predicate (fromRestriction $ head res)) [Variable 'x']) (Atomic (Predicate (fromRestriction $ head res)) [Variable 'y'])) (Atomic (Predicate (fromRestriction $ head res)) [Variable 'z']) ))))]
  AtMost1 res exs -> Right [Universal 'x' $ Universal 'y' (Conditional (Conjunction (Atomic (Predicate (fromRestriction $ head res) ) [Variable 'x'])(Atomic (Predicate (fromRestriction $ head res) ) [Variable 'y'])) (Atomic (Predicate 'I') [Variable 'x',Variable 'y']))]
  AtMost2 res exs -> Right [Universal 'x' $ Universal 'y' $ Universal 'z' (Conditional (Conjunction (Conjunction (Atomic (Predicate (fromRestriction $ head res) ) [Variable 'x'])(Atomic (Predicate (fromRestriction $ head res) ) [Variable 'y']))(Atomic (Predicate (fromRestriction $ head res) ) [Variable 'z'])) (Conjunction (Conjunction (Atomic (Predicate 'I') [Variable 'x',Variable 'y'])(Atomic (Predicate 'I') [Variable 'y',Variable 'z']))(Atomic (Predicate 'I') [Variable 'x',Variable 'z'])) )]
  AtMost3 res exs -> Left "Sorry, too hard to translate."
  Exactly1 res exs -> Right [Conjunction (Existential 'x' (Atomic (Predicate (fromRestriction $ head res)) [Variable 'x'])) (Universal 'x' $ Universal 'y' (Conditional (Conjunction (Atomic (Predicate (fromRestriction $ head res) ) [Variable 'x'])(Atomic (Predicate (fromRestriction $ head res) ) [Variable 'y'])) (Atomic (Predicate 'I') [Variable 'x',Variable 'y'])))]
  Exactly2 res exs -> Right [Conjunction (Existential 'x' (Existential 'y' (Conjunction (Negation (Atomic (Predicate 'I') [Variable 'x',Variable 'y'])) (Conjunction (Atomic (Predicate (fromRestriction $ head res)) [Variable 'x']) (Atomic (Predicate (fromRestriction $ head res)) [Variable 'y']) )))) (Universal 'x' $ Universal 'y' $ Universal 'z' (Conditional (Conjunction (Conjunction (Atomic (Predicate (fromRestriction $ head res) ) [Variable 'x'])(Atomic (Predicate (fromRestriction $ head res) ) [Variable 'y']))(Atomic (Predicate (fromRestriction $ head res) ) [Variable 'z'])) (Conjunction (Conjunction (Atomic (Predicate 'I') [Variable 'x',Variable 'y'])(Atomic (Predicate 'I') [Variable 'y',Variable 'z']))(Atomic (Predicate 'I') [Variable 'x',Variable 'z'])) ))]
  Exactly3 res exs -> Left "Sorry, too hard to translate."


-- lexical lookup functions

fromExclusion :: Exclusion -> Char
fromExclusion (Exclusion x) = lkup x noungloss

fromRestriction :: Restriction -> Char
fromRestriction (Restriction x) = fromcngloss x

fromcngloss :: String -> Char
fromcngloss x = lkup x cngloss

cngloss :: [(String, Char)]
cngloss = [("boy",'B')
          ,("girl",'G')
          ,("person",'P')
          ,("boys",'B')
          ,("girls",'G')
          ,("persons",'P')
          ,("onlooker",'O')
          ,("onlookers", 'O')
          ,("traveller", 'T')
          ,("travellers", 'T')]

fromPred :: Pred -> String
fromPred (Pred x) = x

fromTerms :: [Trm] -> [Term]
fromTerms = map fromTerm

fromTerm :: Trm -> Term
fromTerm x = case x of
  Const s -> Constant (lkup s noungloss)
  Var n -> Variable n


fromverbgloss :: String -> Char
fromverbgloss x = lkup x verbgloss

names :: [[Char]]
names = ["Ashley","Blaire","Clem","Danny","Eden","Frances","Glenn","Harley","Indigo","Jamie","Kris","Luca","Morgan","Natt","Parker","Reagan","Sam","Taylor","Vick","Winter","Zane"]

noungloss :: [([Char], Char)]
noungloss = zip names (map (toLower . head) names)

verbgloss = [("running",'R')
            ,("jumping",'J')
            ,("sitting",'S')
            ,("clapping", 'C')
            ,("drumming", 'D')
            ,("escaping", 'E')
            ,("frightened", 'F')
            ,("mapmaking",'M')
            ,("quiltmaking",'Q')
            ,("happy",'H')
            ,("sad", 'S')
            ,("keen", 'K')
            ,("loves",'L')
            ,("hates",'H')
            ,("love", 'L')
            ,("hate", 'H')
            ,("need", 'N')
            ,("needs", 'N')
            ,("understands", 'U')
            ,("understand", 'U')
            ,("vacillating", 'V')
            ,("waiting", 'W')
            ,("xylophoning",'X')
            ,("yelling",'Y')
            ,("ziplining",'Z')
            ]

lkup :: Eq a => a -> [(a,Char)] -> Char
lkup k xs = fromJust $ lookup k xs

addQuants :: [PosQuant] -> Prop -> Either String [Prop]
addQuants [] p = Right [p]
addQuants [x] p = case x of
  SubQuant qu -> case qu of
    Uni res exs -> case res of
      [] -> case exs of
        [] -> Right [Universal 'x' p]
        [ex] -> Right [Universal 'x' (Conditional (Negation $ Atomic (Predicate 'I') [Variable 'x', Constant $ fromExclusion ex]) p)]
        _ -> Left "Oops something went wrong"
      [res] -> case exs of
        [] -> Right [Universal 'x' (Conditional (Atomic (Predicate (fromRestriction res)) [Variable 'x']) p )]
        [ex] -> Right [Universal 'x' (Conditional (Conjunction (Atomic (Predicate (fromRestriction res)) [Variable 'x']) (Negation $ Atomic (Predicate 'I') [Variable 'x', Constant $ fromExclusion ex])) p )]
        _ -> Left "Oops something went wrong"
      _ -> Left "Oops something went wrong"
    Exi res exs -> case res of
      [] -> case exs of
        [] -> Right [Existential 'x' p]
        [ex] -> Right [Existential 'x' (Conjunction (Negation $ Atomic (Predicate 'I') [Variable 'x', Constant $ fromExclusion ex]) p)]
        _ -> Left "Oops something went wrong"
      [res] -> case exs of
        [] -> Right [Existential 'x' (Conjunction (Atomic (Predicate (fromRestriction res)) [Variable 'x']) p )]
        [ex] -> Right [Existential 'x' (Conjunction (Conjunction (Atomic (Predicate (fromRestriction res)) [Variable 'x']) (Negation $ Atomic (Predicate 'I') [Variable 'x', Constant $ fromExclusion ex])) p )]
        _ -> Left "Oops something went wrong"
      _ -> Left "Oops something went wrong"
    NegExi res exs -> case res of
      [] -> case exs of
        [] -> Right [Negation $ Existential 'x' p]
        [ex] -> Right [Negation $ Existential 'x' (Conjunction (Negation $ Atomic (Predicate 'I') [Variable 'x', Constant $ fromExclusion ex]) p)]
        _ -> Left "Oops something went wrong"
      [res] -> case exs of
        [] -> Right [Negation $ Existential 'x' (Conjunction (Atomic (Predicate (fromRestriction res)) [Variable 'x']) p )]
        [ex] -> Right [Negation $ Existential 'x' (Conjunction (Conjunction (Atomic (Predicate (fromRestriction res)) [Variable 'x']) (Negation $ Atomic (Predicate 'I') [Variable 'x', Constant $ fromExclusion ex])) p )]
        _ -> Left "Oops something went wrong"
      _ -> Left "Oops something went wrong"
    AtLeast2 res exs -> case res of
      [] -> case exs of
        [] -> Left "I can't translate that combination of quantifiers"
        [ex] -> Left "I can't translate that combination of quantifiers"
        _ -> Left "Oops something went wrong"
      [res] -> case exs of
        [] -> do
          a <- getArgofProp p
          b <- getPredofProp p
          return ([Existential 'x' $ Existential 'y' $ Conjunction (Conjunction  (Conjunction (Conjunction (Atomic (Predicate (fromRestriction res)) [Variable 'x']) (Atomic (Predicate (fromRestriction res)) [Variable 'y'])) (Negation $ Atomic (Predicate 'I') [Variable 'x',Variable 'y'])) p ) (Atomic (Predicate b) ([Variable 'y'] ++ a)) ])
        [ex] -> Left "I can't translate that combination of quantifiers"
        _ -> Left "Oops something went wrong"
      _ -> Left "Oops something went wrong"
    AtLeast3 res exs -> case res of
      [] -> case exs of
        [] -> Left "I can't translate that combination of quantifiers"
        [ex] -> Left "I can't translate that combination of quantifiers"
        _ -> Left "Oops something went wrong"
      [res] -> case exs of
        [] -> do
          a <- getArgofProp p
          b <- getPredofProp p
          return ([Existential 'x' $ Existential 'y' $ Existential 'z' $ Conjunction ( Conjunction (Conjunction (Conjunction (Conjunction (Conjunction (Atomic (Predicate (fromRestriction res)) [Variable 'x']) (Atomic (Predicate (fromRestriction res)) [Variable 'y'])) (Atomic (Predicate (fromRestriction res)) [Variable 'z'])) (Negation $ Atomic (Predicate 'I') [Variable 'x',Variable 'y'])) p ) (Atomic (Predicate b) ([Variable 'y'] ++ a))) (Atomic (Predicate b) ([Variable 'z'] ++ a)) ])
        [ex] -> Left "I can't translate that combination of quantifiers"
        _ -> Left "Oops something went wrong"
      _ -> Left "Oops something went wrong"
    AtMost1 res exs -> case res of
      [] -> case exs of
        [] -> Left "I can't translate that combination of quantifiers"
        [ex] -> Left "I can't translate that combination of quantifiers"
        _ -> Left "Oops something went wrong"
      [res] -> case exs of
        [] -> do
          a <- getArgofProp p
          b <- getPredofProp p
          return ([Universal 'x' $ Universal 'y' $ Conditional (Conjunction (Conjunction (Conjunction (Atomic (Predicate (fromRestriction res)) [Variable 'x']) (Atomic (Predicate (fromRestriction res)) [Variable 'y'])) (Atomic (Predicate b) ([Variable 'x']++a))) (Atomic (Predicate b) ([Variable 'y']++a))) (Atomic (Predicate 'I')[Variable 'x',Variable 'y'] ) ])
        [ex] -> Left "I can't translate that combination of quantifiers"
        _ -> Left "Oops something went wrong"
      _ -> Left "Oops something went wrong"
    AtMost2 res exs -> case res of
      [] -> case exs of
        [] -> Left "I can't translate that combination of quantifiers"
        [ex] -> Left "I can't translate that combination of quantifiers"
        _ -> Left "Oops something went wrong"
      [res] -> case exs of
        [] -> do
          a <- getArgofProp p
          b <- getPredofProp p
          return ([Universal 'x' $ Universal 'y' $ Universal 'z' $ Conditional (Conjunction (Conjunction (Conjunction (Conjunction (Conjunction (Atomic (Predicate (fromRestriction res)) [Variable 'x']) (Atomic (Predicate (fromRestriction res)) [Variable 'y'])) (Atomic (Predicate (fromRestriction res)) [Variable 'z'])) (Atomic (Predicate b) ([Variable 'x']++a))) (Atomic (Predicate b) ([Variable 'y']++a))) (Atomic (Predicate b) ([Variable 'z']++a))) (Conjunction (Conjunction (Atomic (Predicate 'I')[Variable 'x',Variable 'y'] ) (Atomic (Predicate 'I')[Variable 'x',Variable 'z'] )) (Atomic (Predicate 'I')[Variable 'y',Variable 'z'] )) ])
        [ex] -> Left "I can't translate that combination of quantifiers"
        _ -> Left "Oops something went wrong"
      _ -> Left "Oops something went wrong"
    AtMost3 res exs -> Left "I can't translate that combination of quantifiers"
    Exactly1 res exs -> case res of
      [] -> case exs of
        [] -> Left "I can't translate that combination of quantifiers"
        [ex] -> Left "I can't translate that combination of quantifiers"
        _ -> Left "Oops something went wrong"
      [res] -> case exs of
        [] -> do
          a <- getArgofProp p
          b <- getPredofProp p
          return ([Conjunction (Existential 'x' (Conjunction (Atomic (Predicate (fromRestriction res)) [Variable 'x']) p )) (Universal 'x' $ Universal 'y' $ Conditional (Conjunction (Conjunction (Conjunction (Atomic (Predicate (fromRestriction res)) [Variable 'x']) (Atomic (Predicate (fromRestriction res)) [Variable 'y'])) (Atomic (Predicate b) ([Variable 'x']++a))) (Atomic (Predicate b) ([Variable 'y']++a))) (Atomic (Predicate 'I')[Variable 'x',Variable 'y'] ) )])
        [ex] -> Left "I can't translate that combination of quantifiers"
        _ -> Left "Oops something went wrong"
      _ -> Left "Oops something went wrong"
    Exactly2 res exs -> case res of
      [] -> case exs of
        [] -> Left "I can't translate that combination of quantifiers"
        [ex] -> Left "I can't translate that combination of quantifiers"
        _ -> Left "Oops something went wrong"
      [res] -> case exs of
        [] -> do
          a <- getArgofProp p
          b <- getPredofProp p
          return ( [Conjunction (Existential 'x' $ Existential 'y' $ Conjunction (Conjunction  (Conjunction (Conjunction (Atomic (Predicate (fromRestriction res)) [Variable 'x']) (Atomic (Predicate (fromRestriction res)) [Variable 'y'])) (Negation $ Atomic (Predicate 'I') [Variable 'x',Variable 'y'])) p ) (Atomic (Predicate b) ([Variable 'y'] ++ a)) ) (Universal 'x' $ Universal 'y' $ Universal 'z' $ Conditional (Conjunction (Conjunction (Conjunction (Conjunction (Conjunction (Atomic (Predicate (fromRestriction res)) [Variable 'x']) (Atomic (Predicate (fromRestriction res)) [Variable 'y'])) (Atomic (Predicate (fromRestriction res)) [Variable 'z'])) (Atomic (Predicate b) ([Variable 'x']++a))) (Atomic (Predicate b) ([Variable 'y']++a))) (Atomic (Predicate b) ([Variable 'z']++a))) (Conjunction (Conjunction (Atomic (Predicate 'I')[Variable 'x',Variable 'y'] ) (Atomic (Predicate 'I')[Variable 'x',Variable 'z'] )) (Atomic (Predicate 'I')[Variable 'y',Variable 'z'] ))) ])
        [ex] -> Left "I can't translate that combination of quantifiers"
        _ -> Left "Oops something went wrong"
      _ -> Left "Oops something went wrong"
    Exactly3 res exs -> Left "I can't translate that combination of quantifiers"
  ObjQuant qu -> case qu of
    Uni res exs -> case res of
      [] -> case exs of
        [] -> Right [Universal 'y' p]
        [ex] -> Right [Universal 'y' (Conditional (Negation $ Atomic (Predicate 'I') [Variable 'y', Constant $ fromExclusion ex]) p)]
        _ -> Left "Oops something went wrong"
      [res] -> case exs of
        [] -> Right [Universal 'y' (Conditional (Atomic (Predicate (fromRestriction res)) [Variable 'y']) p )]
        [ex] -> Right [Universal 'y' (Conditional (Conjunction (Atomic (Predicate (fromRestriction res)) [Variable 'y']) (Negation $ Atomic (Predicate 'I') [Variable 'y', Constant $ fromExclusion ex])) p )]
        _ -> Left "Oops something went wrong"
      _ -> Left "Oops something went wrong"
    Exi res exs -> case res of
      [] -> case exs of
        [] -> Right [Existential 'y' p]
        [ex] -> Right [Existential 'y' (Conjunction (Negation $ Atomic (Predicate 'I') [Variable 'y', Constant $ fromExclusion ex]) p)]
        _ -> Left "Oops something went wrong"
      [res] -> case exs of
        [] -> Right [Existential 'y' (Conjunction (Atomic (Predicate (fromRestriction res)) [Variable 'y']) p )]
        [ex] -> Right [Existential 'y' (Conjunction (Conjunction (Atomic (Predicate (fromRestriction res)) [Variable 'y']) (Negation $ Atomic (Predicate 'I') [Variable 'y', Constant $ fromExclusion ex])) p )]
        _ -> Left "Oops something went wrong"
      _ -> Left "Oops something went wrong"
    NegExi res exs -> case res of
      [] -> case exs of
        [] -> Right [Negation $ Existential 'y' p]
        [ex] -> Right [Negation $ Existential 'y' (Conjunction (Negation $ Atomic (Predicate 'I') [Variable 'y', Constant $ fromExclusion ex]) p)]
        _ -> Left "Oops something went wrong"
      [res] -> case exs of
        [] -> Right [Negation $ Existential 'y' (Conjunction (Atomic (Predicate (fromRestriction res)) [Variable 'y']) p )]
        [ex] -> Right [Negation $ Existential 'y' (Conjunction (Conjunction (Atomic (Predicate (fromRestriction res)) [Variable 'y']) (Negation $ Atomic (Predicate 'I') [Variable 'y', Constant $ fromExclusion ex])) p )]
        _ -> Left "Oops something went wrong"
      _ -> Left "Oops something went wrong"
    AtLeast2 res exs -> case res of
      [] -> case exs of
        [] -> Left "I can't translate that combination of quantifiers"
        [ex] -> Left "I can't translate that combination of quantifiers"
        _ -> Left "Oops something went wrong"
      [res] -> case exs of
        [] -> do
          a <- getArgofProp p
          b <- getPredofProp p
          return ( [Existential 'y' $ Existential 'z' $ Conjunction (Conjunction  (Conjunction (Conjunction (Atomic (Predicate (fromRestriction res)) [Variable 'y']) (Atomic (Predicate (fromRestriction res)) [Variable 'z'])) (Negation $ Atomic (Predicate 'I') [Variable 'y',Variable 'z'])) p ) (Atomic (Predicate b) (a ++ [Variable 'z'])) ])
        [ex] -> Left "I can't translate that combination of quantifiers"
        _ -> Left "Oops something went wrong"
      _ -> Left "Oops something went wrong"
    AtLeast3 res exs -> case res of
      [] -> case exs of
        [] -> Left "I can't translate that combination of quantifiers"
        [ex] -> Left "I can't translate that combination of quantifiers"
        _ -> Left "Oops something went wrong"
      [res] -> case exs of
        [] -> do
          a <- getArgofProp p
          b <- getPredofProp p
          return ([Existential 'y' $ Existential 'z' $ Existential 'u' $ Conjunction (Conjunction ( Conjunction ( Conjunction (Conjunction (Conjunction (Conjunction (Conjunction (Atomic (Predicate (fromRestriction res)) [Variable 'y']) (Atomic (Predicate (fromRestriction res)) [Variable 'z'])) (Atomic (Predicate (fromRestriction res)) [Variable 'u'])) (Negation $ Atomic (Predicate 'I') [Variable 'y',Variable 'z'])) (Negation $ Atomic (Predicate 'I') [Variable 'z',Variable 'u'])) (Negation $ Atomic (Predicate 'I') [Variable 'y',Variable 'u'])) p ) (Atomic (Predicate b) (a++[Variable 'z']))) (Atomic (Predicate b) (a++[Variable 'u'])) ])
        [ex] -> Left "I can't translate that combination of quantifiers"
        _ -> Left "Oops something went wrong"
      _ -> Left "Oops something went wrong"
    AtMost1 res exs -> case res of
      [] -> case exs of
        [] -> Left "I can't translate that combination of quantifiers"
        [ex] -> Left "I can't translate that combination of quantifiers"
        _ -> Left "Oops something went wrong"
      [res] -> case exs of
        [] -> do
          a <- getArgofProp p
          b <- getPredofProp p
          return ([Universal 'x' $ Universal 'y' $ Conditional (Conjunction (Conjunction (Conjunction (Atomic (Predicate (fromRestriction res)) [Variable 'x']) (Atomic (Predicate (fromRestriction res)) [Variable 'y'])) (Atomic (Predicate b) (a++[Variable 'x']))) (Atomic (Predicate b) (a++[Variable 'y']))) (Atomic (Predicate 'I')[Variable 'x',Variable 'y'] ) ])
        [ex] -> Left "I can't translate that combination of quantifiers"
        _ -> Left "Oops something went wrong"
      _ -> Left "Oops something went wrong"
    AtMost2 res exs -> case res of
      [] -> case exs of
        [] -> Left "I can't translate that combination of quantifiers"
        [ex] -> Left "I can't translate that combination of quantifiers"
        _ -> Left "Oops something went wrong"
      [res] -> case exs of
        [] -> do
          a <- getArgofProp p
          b <- getPredofProp p
          return ([Universal 'x' $ Universal 'y' $ Universal 'z' $ Conditional (Conjunction (Conjunction (Conjunction (Conjunction (Conjunction (Atomic (Predicate (fromRestriction res)) [Variable 'x']) (Atomic (Predicate (fromRestriction res)) [Variable 'y'])) (Atomic (Predicate (fromRestriction res)) [Variable 'z'])) (Atomic (Predicate b) (a++[Variable 'x']))) (Atomic (Predicate b) (a ++ [Variable 'y']))) (Atomic (Predicate b) (a ++ [Variable 'z']))) (Conjunction (Conjunction (Atomic (Predicate 'I')[Variable 'x',Variable 'y'] ) (Atomic (Predicate 'I')[Variable 'x',Variable 'z'] )) (Atomic (Predicate 'I')[Variable 'y',Variable 'z'] )) ])
        [ex] -> Left "I can't translate that combination of quantifiers"
        _ -> Left "Oops something went wrong"
      _ -> Left "Oops something went wrong"
    AtMost3 res exs -> Left "I can't translate that combination of quantifiers"
    Exactly1 res exs -> case res of
      [] -> case exs of
        [] -> Left "I can't translate that combination of quantifiers"
        [ex] -> Left "I can't translate that combination of quantifiers"
        _ -> Left "Oops something went wrong"
      [res] -> case exs of
        [] -> do
          a <- getArgofProp p
          b <- getPredofProp p
          return ([Conjunction (Existential 'x' (Conjunction (Atomic (Predicate (fromRestriction res)) [Variable 'x']) p )) (Universal 'x' $ Universal 'y' $ Conditional (Conjunction (Conjunction (Conjunction (Atomic (Predicate (fromRestriction res)) [Variable 'x']) (Atomic (Predicate (fromRestriction res)) [Variable 'y'])) (Atomic (Predicate b) (a++[Variable 'x']))) (Atomic (Predicate b) (a++[Variable 'y']))) (Atomic (Predicate 'I')[Variable 'x',Variable 'y'] ) ) ])
        [ex] -> Left "I can't translate that combination of quantifiers"
        _ -> Left "Oops something went wrong"
      _ -> Left "Oops something went wrong"
    Exactly2 res exs -> case res of
      [] -> case exs of
        [] -> Left "I can't translate that combination of quantifiers"
        [ex] -> Left "I can't translate that combination of quantifiers"
        _ -> Left "Oops something went wrong"
      [res] -> case exs of
        [] -> do
          a <- getArgofProp p
          b <- getPredofProp p
          return ([Conjunction (Existential 'y' $ Existential 'z' $ Conjunction (Conjunction  (Conjunction (Conjunction (Atomic (Predicate (fromRestriction res)) [Variable 'y']) (Atomic (Predicate (fromRestriction res)) [Variable 'z'])) (Negation $ Atomic (Predicate 'I') [Variable 'y',Variable 'z'])) p ) (Atomic (Predicate b) (a ++ [Variable 'z'])) ) (Universal 'x' $ Universal 'y' $ Universal 'z' $ Conditional (Conjunction (Conjunction (Conjunction (Conjunction (Conjunction (Atomic (Predicate (fromRestriction res)) [Variable 'x']) (Atomic (Predicate (fromRestriction res)) [Variable 'y'])) (Atomic (Predicate (fromRestriction res)) [Variable 'z'])) (Atomic (Predicate b) (a++[Variable 'x']))) (Atomic (Predicate b) (a ++ [Variable 'y']))) (Atomic (Predicate b) (a ++ [Variable 'z']))) (Conjunction (Conjunction (Atomic (Predicate 'I')[Variable 'x',Variable 'y'] ) (Atomic (Predicate 'I')[Variable 'x',Variable 'z'] )) (Atomic (Predicate 'I')[Variable 'y',Variable 'z'] ))) ])
        [ex] -> Left "I can't translate that combination of quantifiers"
        _ -> Left "Oops something went wrong"
      _ -> Left "Oops something went wrong"
    Exactly3 res exs -> Left "I can't translate that combination of quantifiers"
addQuants [x,y] p = do
  a <- addQuants [y] p
  b <- addQuants [x] p
  z <- addQuants [x] (head a)
  u <- addQuants [y] (head b)
  return (z ++ u)
addQuants _ p = Left "Oops, something went wrong"


getPredofProp :: Prop -> Either String Char
getPredofProp p = case p of
  Atomic (Predicate x) tes -> Right x
  Negation pr -> getPredofProp pr
  Existential c pr -> Left "I can't translate this combination of quantifiers (only one numerical quantifier per clause, please)."
  Universal c pr -> Left "I can't translate this combination of quantifiers (only one numerical quantifier per clause, please)."
  Conjunction pr pr' -> Left "I can't translate this combination of quantifiers (only one numerical quantifier per clause, please)."
  Disjunction pr pr' -> Left "I can't translate this combination of quantifiers (only one numerical quantifier per clause, please)."
  Conditional pr pr' -> Left "I can't translate this combination of quantifiers (only one numerical quantifier per clause, please)."
  Biconditional pr pr' -> Left "I can't translate this combination of quantifiers (only one numerical quantifier per clause, please)."

getArgofProp :: Prop -> Either String [Term]
getArgofProp p = case p of
  Atomic (Predicate x) tes -> Right $ onlyConsts tes
  Negation pr -> getArgofProp pr
  Existential c pr -> Left "I can't translate this combination of quantifiers (only one numerical quantifier per clause, please)."
  Universal c pr -> Left "I can't translate this combination of quantifiers (only one numerical quantifier per clause, please)."
  Conjunction pr pr' -> Left "I can't translate this combination of quantifiers (only one numerical quantifier per clause, please)."
  Disjunction pr pr' -> Left "I can't translate this combination of quantifiers (only one numerical quantifier per clause, please)."
  Conditional pr pr' -> Left "I can't translate this combination of quantifiers (only one numerical quantifier per clause, please)."
  Biconditional pr pr' -> Left "I can't translate this combination of quantifiers (only one numerical quantifier per clause, please)."

onlyConsts :: [Term] -> [Term]
onlyConsts [] = []
onlyConsts (Constant x:xs) = Constant x : onlyConsts xs
onlyConsts (Variable x:xs) = onlyConsts xs