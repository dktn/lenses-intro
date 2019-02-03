module Main where

import qualified Example1
import qualified Example2
import qualified Example3
import qualified Example4
import qualified Example5

main :: IO ()
main = do
  Example1.run
  Example2.run
  Example3.run
  Example4.run
  Example5.run
