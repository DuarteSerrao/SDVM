module Main where

import Generator (genProg)
import Test.QuickCheck.Gen(sample)


main :: IO ()
main = sample genProg
