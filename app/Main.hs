module Main (main) where

import Control.Concurrent.Async ( async, wait )

import MakePS.MakePS01 ( mkps01 )
import MakePS.MakePS02 ( mkps02 )
import MakePS.MakePS04 ( mkps04 )
import MakePS.MakePS07 ( mkps07 )
import MakePS.MakePS08 ( mkps08g )
import MakePS.MakePS09 ( mkps09 )
import MakePS.MakePS10 ( mkps10 )

import System.Random ( newStdGen )

main :: IO ()
main = do 
       gen8 <- newStdGen
       a <- async mkps01r
       b <- async mkps02r
       c <- async mkps04r
       d <- async mkps07r
       g <- async (mkps08r gen8)
       e <- async mkps09r
       f <- async mkps10r
       a1 <- wait a
       b1 <- wait b
       c1 <- wait c
       d1 <- wait d
       e1 <- wait e
       f1 <- wait f
       g1 <- wait g
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

mkps08r gen = do
              mkps08g gen
              putStrLn "PS08 Done!"

mkps04r = do
          mkps04
          putStrLn "PS04 Done!"


mkps02r = do
          mkps02
          putStrLn "PS02 Done!"


mkps01r = do
          mkps01
          putStrLn "PS01 Done!"

