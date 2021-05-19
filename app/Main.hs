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

basic :: [Int] -> IO ()
basic ns = do
       g <- newStdGen    -- get random generator
       let (num,_) = next g  -- use it to get a random number
       let seed = mkStdGen num
       mapConcurrently_ id (get ns (allsets seed num))
       return()

batch :: Int -> [Int] -> IO ()
batch n ns =  replicateConcurrently_ n (basic ns)

basic2 :: [Int] -> Int -> IO ()
basic2 ns num = do
          let seed = mkStdGen num
          mapConcurrently_ id (get ns (allsets seed num))
          return()

allsets seed num = [mkps01g seed num, mkps02g seed num, mkps02g seed num, mkps04g seed num, mkps02g seed num, mkps02g seed num, mkps07g seed num, mkps08g seed num, mkps09g seed num, mkps10g seed num]

get ns xs = map (\n -> (fromJust $ lookup n (zip [1..] xs))) ns

batch2 :: Int -> [Int] -> [Int] -> IO ()
batch2 n1 ns n2 = mapConcurrently_ (\x -> replicateConcurrently_ n1 (basic2 ns x)) n2

main :: IO ()
main = mode =<< execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "problemsets 0.1.0.0"
     <> header "problemsets - generate problem sets for logic" )

mode :: MyOptions -> IO ()
mode opts | not (null (ident opts)) = batch2 (bsize opts) (psets opts) (ident opts)
          | otherwise =  batch (bsize opts) (psets opts)

data MyOptions = MyOptions
  { bsize      :: Int
  , psets      :: [Int]
  , ident      :: [Int]
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
         <> value [1,2,4,7,8,9,10]
         <> metavar "[INT]" )
      <*> option auto
          ( long "identifier"
         <> short 'i'
         <> help "Problem set identifier"
         <> showDefault
         <> value []
         <> metavar "[INT]" )


