module Mutations (mutProg, exampleTree, genIndex) where

import CDP
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Prelude hiding (GT, LT)
import Optimize
import Library.Ztrategic
import Data.Generics.Zipper
import Data.Maybe
import System.IO.Unsafe


exampleTree = Prog [Fun CharDenotation "s" [] [Assign "x" (Add (Mul (LT (Const 1) (Const 2)) (GT (Const 3) (Const 4))) (EQU (Const 5) (DIF (Const 6) (Const 7))))]]


genIndex :: Int
genIndex = unsafePerformIO (generate (choose (0, 5)))

mutProg :: Prog -> Prog
mutProg prog = fromZipper (fromJust (applyTP (once_tdTP step) (toZipper prog)))
    where step = failTP `adhocTP` mutate


mutate :: Exp -> Maybe Exp
mutate exp = (mutations !! genIndex) exp
    

mutations = [   mut_Add2Mul, 
                mut_Mul2Add, 
                mut_GT2LT, 
                mut_LT2GT, 
                mut_EQU2DIF, 
                mut_DIF2EQU
            ]


-- Mutation 1
mut_Add2Mul :: Exp -> Maybe Exp
mut_Add2Mul (Add exp1 exp2) = Just (Mul exp1 exp2)
mut_Add2Mul _ = Nothing

-- Mutation 2
mut_Mul2Add :: Exp -> Maybe Exp
mut_Mul2Add (Add exp1 exp2) = Just (Mul exp1 exp2)
mut_Mul2Add _ = Nothing

-- Mutation 3
mut_GT2LT :: Exp -> Maybe Exp
mut_GT2LT (GT exp1 exp2) = Just (LT exp1 exp2)
mut_GT2LT _ = Nothing

-- Mutation 4
mut_LT2GT :: Exp -> Maybe Exp
mut_LT2GT (LT exp1 exp2) = Just (GT exp1 exp2)
mut_LT2GT _ = Nothing

-- Mutation 5
mut_EQU2DIF :: Exp -> Maybe Exp
mut_EQU2DIF (EQU exp1 exp2) = Just (DIF exp1 exp2)
mut_EQU2DIF _ = Nothing

-- Mutation 6
mut_DIF2EQU :: Exp -> Maybe Exp
mut_DIF2EQU (DIF exp1 exp2) = Just (EQU exp1 exp2)
mut_DIF2EQU _ = Nothing