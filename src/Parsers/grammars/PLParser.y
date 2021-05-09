{
module HappyParser (happyParser) where
import Token
import Props (Prop (..))
}

%name happyParser
%tokentype { Token }
%error { parseError }

%token
    basic        { BasicSymbol $$ }
    "~"          { NegationSymbol }
    "&"          { ConjunctionSymbol }
    "v"          { DisjunctionSymbol }
    "->"         { ConditionalSymbol }
    "<->"        { BiconditionalSymbol }
    "("          { LeftPar }
    ")"          { RightPar }

%%

Prop : basic         { Basic $1 }
     | "~" Prop                  { Negation $2 } 
     | "(" Prop "&" Prop ")"     { Conjunction $2 $4 }  
     | "(" Prop "v" Prop ")"     { Disjunction $2 $4 }
     | "(" Prop "->" Prop ")"    { Conditional $2 $4 }
     | "(" Prop "<->" Prop ")"   { Biconditional $2 $4 }

{

parseError :: [Token] -> a
parseError _ = error "Parse Error"

}

