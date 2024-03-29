-- Haskell module generated by the BNF converter

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module NewTranslations.EnglishL.Skel where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified NewTranslations.EnglishL.Abs as EnglishL.Abs

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transNoun :: EnglishL.Abs.Noun -> Result
transNoun x = case x of
  EnglishL.Abs.Noun string -> failure x

transCNs :: EnglishL.Abs.CNs -> Result
transCNs x = case x of
  EnglishL.Abs.CNs string -> failure x

transCN :: EnglishL.Abs.CN -> Result
transCN x = case x of
  EnglishL.Abs.CN string -> failure x

transIVerb :: EnglishL.Abs.IVerb -> Result
transIVerb x = case x of
  EnglishL.Abs.IVerb string -> failure x

transTVerb :: EnglishL.Abs.TVerb -> Result
transTVerb x = case x of
  EnglishL.Abs.TVerb string -> failure x

transAdj :: EnglishL.Abs.Adj -> Result
transAdj x = case x of
  EnglishL.Abs.Adj string -> failure x

transClause :: EnglishL.Abs.Clause -> Result
transClause x = case x of
  EnglishL.Abs.ClauseS nounphrases verbphrases -> failure x
  EnglishL.Abs.ClauseP nounphrasep verbphrasep -> failure x
  EnglishL.Abs.Cond1 clause1 clause2 -> failure x
  EnglishL.Abs.Cond2 clause1 clause2 -> failure x
  EnglishL.Abs.Cond3 clause1 clause2 -> failure x
  EnglishL.Abs.Cond4 clause1 clause2 -> failure x
  EnglishL.Abs.BCon1 clause1 clause2 -> failure x
  EnglishL.Abs.Conj1 clause1 clause2 -> failure x
  EnglishL.Abs.Conj2 clause1 clause2 -> failure x
  EnglishL.Abs.Disj1 clause1 clause2 -> failure x
  EnglishL.Abs.Disj2 clause1 clause2 -> failure x
  EnglishL.Abs.CNeg1 clause -> failure x
  EnglishL.Abs.CNeg2 clause -> failure x
  EnglishL.Abs.CNeg3 clause -> failure x
  EnglishL.Abs.Spec1 clause1 clause2 -> failure x
  EnglishL.Abs.Spec2 clause1 clause2 -> failure x
  EnglishL.Abs.Spec3 clause1 clause2 -> failure x
  EnglishL.Abs.Spec4 clause1 clause2 -> failure x
  EnglishL.Abs.Spec5 clause1 clause2 -> failure x
  EnglishL.Abs.Spec6 clause1 clause2 -> failure x
  EnglishL.Abs.Spec7 clause1 clause2 -> failure x
  EnglishL.Abs.Spec8 clause1 clause2 -> failure x
  EnglishL.Abs.Spec9 clause invclause -> failure x
  EnglishL.Abs.Spec10 clause invclause -> failure x
  EnglishL.Abs.ExeS nquantps -> failure x
  EnglishL.Abs.ExeP nquantpp -> failure x

transInvClause :: EnglishL.Abs.InvClause -> Result
transInvClause x = case x of
  EnglishL.Abs.InvClause1 nounphrases verbphrase -> failure x
  EnglishL.Abs.InvClause2 nounphrasep verbphrase -> failure x

transVerbPhraseP :: EnglishL.Abs.VerbPhraseP -> Result
transVerbPhraseP x = case x of
  EnglishL.Abs.VPP1 verbphrase -> failure x
  EnglishL.Abs.VPP2 adjphrase -> failure x
  EnglishL.Abs.VPP3 verbphrase -> failure x
  EnglishL.Abs.VPP1Neg verbphrase -> failure x
  EnglishL.Abs.VPP2Neg adjphrase -> failure x
  EnglishL.Abs.VPP1NegC verbphrase -> failure x
  EnglishL.Abs.VPP2NegC adjphrase -> failure x

transVerbPhraseS :: EnglishL.Abs.VerbPhraseS -> Result
transVerbPhraseS x = case x of
  EnglishL.Abs.VPS1 verbphrase -> failure x
  EnglishL.Abs.VPS2 adjphrase -> failure x
  EnglishL.Abs.VPS3 verbphrase -> failure x
  EnglishL.Abs.VPS1Neg verbphrase -> failure x
  EnglishL.Abs.VPS2Neg adjphrase -> failure x
  EnglishL.Abs.VPS1NegC verbphrase -> failure x
  EnglishL.Abs.VPS2NegC adjphrase -> failure x

transVerbPhrase :: EnglishL.Abs.VerbPhrase -> Result
transVerbPhrase x = case x of
  EnglishL.Abs.VP1 verbx -> failure x
  EnglishL.Abs.VPConj1 verbx1 verbx2 -> failure x
  EnglishL.Abs.VPConj2 verbx1 verbx2 -> failure x
  EnglishL.Abs.VPDisj1 verbx1 verbx2 -> failure x
  EnglishL.Abs.VPDisj2 verbx1 verbx2 -> failure x
  EnglishL.Abs.VPNegDisj1 verbx1 verbx2 -> failure x

transVerbX :: EnglishL.Abs.VerbX -> Result
transVerbX x = case x of
  EnglishL.Abs.VerbX1 iverb -> failure x
  EnglishL.Abs.VerbX2 tverb nounphrasep -> failure x
  EnglishL.Abs.VerbX3 tverb nounphrases -> failure x
  EnglishL.Abs.VerbXR1 tverb -> failure x
  EnglishL.Abs.VerbXR2 tverb -> failure x
  EnglishL.Abs.VerbxR3 tverb -> failure x

transAdjPhrase :: EnglishL.Abs.AdjPhrase -> Result
transAdjPhrase x = case x of
  EnglishL.Abs.AdjP1 adj -> failure x
  EnglishL.Abs.AdjPConj1 adj1 adj2 -> failure x
  EnglishL.Abs.AdjPConj2 adj1 adj2 -> failure x
  EnglishL.Abs.AdjPDisj1 adj1 adj2 -> failure x
  EnglishL.Abs.AdjPDisj2 adj1 adj2 -> failure x
  EnglishL.Abs.AdjPNegDisj1 adj1 adj2 -> failure x

transNounPhraseS :: EnglishL.Abs.NounPhraseS -> Result
transNounPhraseS x = case x of
  EnglishL.Abs.NPS1 noun -> failure x
  EnglishL.Abs.NPS2 quantps -> failure x
  EnglishL.Abs.NPS3 nquantps -> failure x

transNounPhraseP :: EnglishL.Abs.NounPhraseP -> Result
transNounPhraseP x = case x of
  EnglishL.Abs.NPPConj1 noun1 noun2 -> failure x
  EnglishL.Abs.NPPConj2 noun1 noun2 -> failure x
  EnglishL.Abs.NPPQuan1 quantpp -> failure x
  EnglishL.Abs.NPPQuan2 nquantpp -> failure x

transQuantPS :: EnglishL.Abs.QuantPS -> Result
transQuantPS x = case x of
  EnglishL.Abs.QuantPS1 -> failure x
  EnglishL.Abs.QuantPS2 -> failure x
  EnglishL.Abs.QuantPS3 -> failure x
  EnglishL.Abs.QuantPS4 -> failure x
  EnglishL.Abs.QuantPS5 -> failure x
  EnglishL.Abs.QuantPS6 -> failure x
  EnglishL.Abs.QuantPS7 -> failure x
  EnglishL.Abs.QuantPS8 -> failure x
  EnglishL.Abs.QuantPS9 -> failure x
  EnglishL.Abs.QuantPSUni cn -> failure x
  EnglishL.Abs.QuantPSExi cn -> failure x
  EnglishL.Abs.QuantPSNeg cn -> failure x
  EnglishL.Abs.QuantSpec1 noun -> failure x
  EnglishL.Abs.QuantSpec2 noun -> failure x
  EnglishL.Abs.QuantSpec3 noun -> failure x
  EnglishL.Abs.QuantSpec4 noun -> failure x
  EnglishL.Abs.QuantSpec5 noun -> failure x
  EnglishL.Abs.QuantSpec6 noun -> failure x
  EnglishL.Abs.QuantSpec7 noun -> failure x
  EnglishL.Abs.QuantSpec8 noun -> failure x
  EnglishL.Abs.QuantSpec9 noun -> failure x
  EnglishL.Abs.QuantSpec10 cn noun -> failure x
  EnglishL.Abs.QuantSpec11 cn noun -> failure x

transNQuantPS :: EnglishL.Abs.NQuantPS -> Result
transNQuantPS x = case x of
  EnglishL.Abs.NQuantPS10 cn -> failure x
  EnglishL.Abs.NQuantPS11 cn -> failure x
  EnglishL.Abs.NQuantPS12 cn -> failure x

transQuantPP :: EnglishL.Abs.QuantPP -> Result
transQuantPP x = case x of
  EnglishL.Abs.QuantPP1 cns -> failure x
  EnglishL.Abs.QuantPP2 cns -> failure x
  EnglishL.Abs.QuantPP3 cns -> failure x

transNQuantPP :: EnglishL.Abs.NQuantPP -> Result
transNQuantPP x = case x of
  EnglishL.Abs.NQuantPP1 cns -> failure x
  EnglishL.Abs.NQuantPP2 cns -> failure x
  EnglishL.Abs.NQuantPP3 cns -> failure x
  EnglishL.Abs.NQuantPP5 cns -> failure x
