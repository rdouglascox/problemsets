module Translations.MonadicParserLeft (monadicLeftParse) where

import Control.Monad (liftM, ap)
import Data.Char
import Data.PLprop

-- parse function

monadicLeftParse :: String -> Prop
monadicLeftParse x = fst((apply parseWff x)!!0)

-- general parser functions

newtype Parser a = Parser (String -> [(a,String)])
apply :: Parser a -> String -> [(a,String)]
apply (Parser p) s = p s

parse :: Parser a -> String -> a
parse p = fst . head . apply p

--- type declaration

instance Monad Parser where
    return x = Parser (\s -> [(x,s)])
    p >>= q  = Parser (\s -> [(y,s'')
                           | (x,s') <- apply p s,
                             (y,s'') <- apply (q x) s'])

instance Functor Parser where
    fmap = liftM

instance Applicative Parser where
    pure = return
    (<*>) = ap

getc :: Parser Char
getc = Parser f
       where f [] = []
             f (c:cs) = [(c,cs)]

sat :: (Char -> Bool) -> Parser Char
sat p = do {c <- getc;
            if p c then return c
            else ffail}

ffail = Parser (\s -> [])

char :: Char -> Parser ()
char x = do {c <- sat (==x); return ()}

string :: String -> Parser ()
string [] = return ()
string (x:xs) = do {char x; string xs; return ()}

--- choice operator for parsers

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = Parser f
          where f s = let ps = apply p s in 
                      if null ps then apply q s
                      else ps

-- parsers for PL

-- our grammar
-- wff   ::= '(' | wff conn wff ')' | '~' wff | basic
-- conn  ::= "&" | "v" | "->" | "<->"
-- basic ::= 'A' | 'B' | ... | 'Z'


parseBasic :: Parser Prop
parseBasic =  do c <- sat (`elem` ['A'..'Z'])
                 return (Basic [c])

parseNegation :: Parser Prop
parseNegation = do c <- sat (=='~')
                   p <- parseWff
                   return (Negation p)

parseLeft :: Parser Prop
parseLeft = do
            _ <- sat (=='(')
            p <- parseWff
            return (p) 


parseConjunctionRight :: Prop -> Parser Prop
parseConjunctionRight x = do 
                          c <- string "&"
                          r <- parseWff
                          _ <- sat (==')')
                          return (Conjunction x r)

parseDisjunctionRight :: Prop -> Parser Prop
parseDisjunctionRight x = do 
                          c <- string "v"
                          r <- parseWff
                          _ <- sat (==')')
                          return (Disjunction x r)

parseConditionalRight :: Prop -> Parser Prop
parseConditionalRight  x = do 
                       c <- string "->"
                       r <- parseWff
                       _ <- sat (==')')
                       return (Conditional x r)

parseBiconditionalRight :: Prop -> Parser Prop
parseBiconditionalRight  x = do 
                         c <- string "<->"
                         r <- parseWff
                         _ <- sat (==')')
                         return (Conditional x r)

parseRight :: Prop -> Parser Prop
parseRight x = do parseConjunctionRight x
               <|> parseConditionalRight x
               <|> parseDisjunctionRight x
               <|> parseBiconditionalRight x

parseBinary :: Parser Prop
parseBinary = do 
              l <- parseLeft
              parseRight l

parseWff :: Parser Prop 
parseWff = do parseBasic 
           <|> do parseNegation
           <|> do parseBinary

