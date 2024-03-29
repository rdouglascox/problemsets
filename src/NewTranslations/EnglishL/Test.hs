-- Program to test parser, automatically generated by BNF Converter.

module NewTranslations.EnglishL.Test where

import Prelude
  ( ($), (.)
  , Either(..)
  , Int, (>)
  , String, (++), concat, unlines
  , Show, show
  , IO, (>>), (>>=), mapM_, putStrLn
  , FilePath
  , getContents, readFile
  , error, flip, map, replicate, sequence_, zip
  )
import System.Environment ( getArgs )
import System.Exit        ( exitFailure, exitSuccess )
import Control.Monad      ( when )

import NewTranslations.EnglishL.Abs   ( Clause )
import NewTranslations.EnglishL.Lex   ( Token, mkPosToken )
import NewTranslations.EnglishL.Par   ( pClause, myLexer, GLRResult(..), Branch, ForestId, TreeDecode(..), decode )
import NewTranslations.EnglishL.Print ( Print, printTree )
import NewTranslations.EnglishL.Skel  ()
import qualified Data.Map ( Map, lookup, toList )
import Data.Maybe ( fromJust )

type Err        = Either String
type ParseFun a = [[Token]] -> (GLRResult, GLR_Output (Err a))
type Verbosity  = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: (TreeDecode a, Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: (TreeDecode a, Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()
run v p s
 = let ts = map (:[]) $ myLexer s
       (raw_output, simple_output) = p ts in
   case simple_output of
     GLR_Fail major minor -> do
                               putStrLn major
                               putStrV v minor
     GLR_Result df trees  -> do
                               putStrLn "\nParse Successful!"
                               case trees of
                                 []        -> error "No results but parse succeeded?"
                                 [Right x] -> showTree v x
                                 xs@(_:_)  -> showSeveralTrees v xs
   where
  showSeveralTrees :: (Print b, Show b) => Int -> [Err b] -> IO ()
  showSeveralTrees v trees
   = sequence_
     [ do putStrV v (replicate 40 '-')
          putStrV v $ "Parse number: " ++ show n
          showTree v t
     | (Right t,n) <- zip trees [1..]
     ]

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree = do
  putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    []         -> getContents >>= run 2 the_parser
    "-s":fs    -> mapM_ (runFile 0 the_parser) fs
    fs         -> mapM_ (runFile 2 the_parser) fs

the_parser :: ParseFun Clause
the_parser = lift_parser pClause

type Forest = Data.Map.Map ForestId [Branch]      -- omitted in ParX export.
data GLR_Output a
 = GLR_Result { pruned_decode     :: (Forest -> Forest) -> [a]
              , semantic_result   :: [a]
              }
 | GLR_Fail   { main_message :: String
              , extra_info   :: String
              }

lift_parser
 :: (TreeDecode a, Show a, Print a)
 => ([[Token]] -> GLRResult) -> ParseFun a
lift_parser parser ts
 = let result = parser ts in
   (\o -> (result, o)) $
   case result of
     ParseError ts f -> GLR_Fail "Parse failed, unexpected token(s)\n"
                                 ("Tokens: " ++ show ts)
     ParseEOF   f    -> GLR_Fail "Parse failed, unexpected EOF\n"
                                 ("Partial forest:\n"
                                    ++ unlines (map show $ Data.Map.toList f))
     ParseOK r f     -> let find   f = fromJust . ((flip Data.Map.lookup) f)
                            dec_fn f = decode (find f) r
                        in GLR_Result (\ff -> dec_fn $ ff f) (dec_fn f)

