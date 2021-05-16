module Main where

import Control.Concurrent.Async ( async, wait, (mkps07g seed num) )
import Control.Monad
import System.Environment

import MakePS.MakePS01 ( mkps01 )
import MakePS.MakePS02 ( mkps02 )
import MakePS.MakePS04 ( mkps04 )
import MakePS.MakePS07 ( mkps07g )
import MakePS.MakePS08 ( mkps08g )
import MakePS.MakePS09 ( mkps09g )
import MakePS.MakePS10 ( mkps10g )

import System.Random ( newStdGen, next, mkStdGen )

main =
       batch 4

basic :: IO ()
basic = do
       g <- newStdGen    -- get random generator
       let (num,_) = next g  -- use it to get a random number
       let seed = mkStdGen num
       mapConcurrently_ id [mkps01,mkps02,mkps04, mkps07g seed num, mkps08g seed num, mkps09g seed num, mkps10g seed num]
       return()


batch :: Int -> IO ()
batch n = replicateM_ n main