module NewTranslations.JustParses (justParses) where

-- | function to return either an empt list or a list of Sentences

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

justParses s = let ts = map (:[]) $ myLexer s
                   (raw_output, simple_output) = the_parser ts in
   case simple_output of
     GLR_Fail major minor -> []
     GLR_Result df trees  -> case trees of
                                 []        -> []
                                 [Right x] -> [x]
                                 xs@(_:_)  -> map gr xs
   where gr (Right x) = x


