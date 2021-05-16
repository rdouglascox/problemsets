module Main where

import Control.Concurrent.Async ( async, wait )

import MakePS.MakePS01 ( mkps01 )
import MakePS.MakePS02 ( mkps02 )
import MakePS.MakePS04 ( mkps04 )
import MakePS.MakePS07 ( mkps07 )
import MakePS.MakePS08 ( mkps08g )
import MakePS.MakePS09 ( mkps09g )
import MakePS.MakePS10 ( mkps10 )

import System.Random ( newStdGen, next, mkStdGen )

main :: IO ()
main = do 
       g <- newStdGen    -- get random generator
       let (num,_) = next g  -- use it to get a random number
       let seed = mkStdGen num
       a <- async mkps01
       b <- async mkps02
       c <- async mkps04
       d <- async mkps07
       g <- async (mkps08g seed num)
       e <- async (mkps09g seed num)
       f <- async mkps10
       a1 <- wait a
       b1 <- wait b
       c1 <- wait c
       d1 <- wait d
       e1 <- wait e
       f1 <- wait f
       g1 <- wait g
       return()


