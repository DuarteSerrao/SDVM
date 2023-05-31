module Main where

import Generator (genExp)
import Test.QuickCheck.Gen(sample)


main :: IO ()
main = sample genExp
