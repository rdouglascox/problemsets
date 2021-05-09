module Main where

import Control.Concurrent.Async

import ProblemSet01.MakePS01
import ProblemSet02.MakePS02
import ProblemSet04.MakePS04
import ProblemSet07.MakePS07
import ProblemSet09.MakePS09
import ProblemSet10.MakePS10

main :: IO ()
main = do 
       a <- async mkps01r
       b <- async mkps02r
       c <- async mkps04r
       d <- async mkps07r
       e <- async mkps09r
       f <- async mkps10r
       a1 <- wait a
       b1 <- wait b
       c1 <- wait c
       d1 <- wait d
       e1 <- wait e
       f1 <- wait f
       putStrLn "All Done!"

mkps10r = do
          mkps10
          putStrLn "PS10 Done!"


mkps09r = do
          mkps09
          putStrLn "PS09 Done!"


mkps07r = do
          mkps07
          putStrLn "PS07 Done!"


mkps04r = do
          mkps04
          putStrLn "PS04 Done!"


mkps02r = do
          mkps02
          putStrLn "PS02 Done!"


mkps01r = do
          mkps01
          putStrLn "PS01 Done!"

