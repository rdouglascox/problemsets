{
module Token where
}

%wrapper "basic"

$alpha = [A-Z]

tokens :-

  $white+                             ;
  $alpha                              { \s -> BasicSymbol s }
  "~"                                 { \s -> NegationSymbol }
  "&"                                 { \s -> ConjunctionSymbol }
  "v"                                 { \s -> DisjunctionSymbol }
  "->"                                { \s -> ConditionalSymbol }
  "<->"                               { \s -> BiconditionalSymbol }
  "("                                 { \s -> LeftPar }
  ")"                                 { \s -> RightPar }
  
{

data GPLIToken = BasicSymbol String 
               | NegationSymbol
               | ConjunctionSymbol
               | DisjunctionSymbol
               | ConditionalSymbol
               | BiconditionalSymbol
               | LeftPar
               | RightPar
               deriving (Eq,Show)

} 
