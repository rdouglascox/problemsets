module Main where

import Control.Concurrent.Async ( async, wait,  mapConcurrently_, replicateConcurrently_ )
import Data.List
import Data.Maybe

import MakePS.MakePS01 ( mkps01g )
import MakePS.MakePS02 ( mkps02g )
import MakePS.MakePS04 ( mkps04g )
import MakePS.MakePS07 ( mkps07g )
import MakePS.MakePS08 ( mkps08g )
import MakePS.MakePS09 ( mkps09g )
import MakePS.MakePS10 ( mkps10g )

import Options.Applicative
    ( (<**>),
      auto,
      fullDesc,
      header,
      help,
      info,
      long,
      metavar,
      option,
      progDesc,
      short,
      showDefault,
      strOption,
      value,
      execParser,
      helper,
      Parser )
import Data.Semigroup ((<>))

import System.Random ( newStdGen, next, mkStdGen )

basic :: IO ()
basic = do
       g <- newStdGen    -- get random generator
       let (num,_) = next g  -- use it to get a random number
       let seed = mkStdGen num
       mapConcurrently_ id (allsets seed num)
       return()

batch :: Int -> IO ()
batch n =  replicateConcurrently_ n basic

basic2 :: Int -> IO ()
basic2 num = do
       let seed = mkStdGen num
       mapConcurrently_ id (allsets seed num)
       return()


allsets seed num = [mkps01g seed num, mkps02g seed num, mkps04g seed num,mkps07g seed num, mkps08g seed num, mkps09g seed num, mkps10g seed num]

get ns xs = map (\n -> (fromJust $ lookup n (zip [1..] xs))) ns

batch2 :: Int -> Int -> IO ()
batch2 n1 n2 =  replicateConcurrently_ n1 (basic2 n2)

main :: IO ()
main = mode =<< execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "problemsets 0.1.0.0"
     <> header "problemsets - generate problem sets for logic" )

mode :: MyOptions -> IO ()
mode opts | ident opts /= 0 = batch2 (bsize opts) (ident opts)
          | otherwise =  batch (bsize opts)

data MyOptions = MyOptions
  { bsize      :: Int
  , psets      :: [Int]
  , ident      :: Int
  , file       :: String
  }

sample :: Parser MyOptions
sample = MyOptions
      <$> option auto
          ( long "batch-size"
         <> short 'b'
         <> help "Batch size"
         <> showDefault
         <> value 1
         <> metavar "INT" )
      <*> option auto
          ( long "problem-sets"
         <> short 'p'
         <> help "Which problem sets"
         <> showDefault
         <> value [1]
         <> metavar "[INT]" )
      <*> option auto
          ( long "identifier"
         <> short 'i'
         <> help "Problem set identifier"
         <> showDefault
         <> value 0
         <> metavar "INT" )
      <*> strOption
          ( long "file"
         <> short 'f'
         <> help "Identifier file"
         <> showDefault
         <> value "identifiers.txt"
         <> metavar "FILENAME" )

