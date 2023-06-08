--------------------------------------------------------------------------------
-- Universidade do Minho
-- Mestrado em Engenharia Informática
-- Perfil SDVM - Manutenção e Evolução de Software
-- ALUNOS: pg50289 - Catarina Martins
--         a83630  - Duarte Serrão
--         pg46542 - Pedro Melo
--------------------------------------------------------------------------------
module Generator(genProg) where

import CDP
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Prelude hiding (GT, LT)
import Control.Monad

instance Arbitrary Prog where
        arbitrary = genProg

namesTaken = []

alphabet :: String
alphabet = ['a' .. 'z']


genName :: Gen String
genName = listOf1 (elements alphabet)

genNum :: Gen Int
genNum = arbitrary


genProg :: Gen Prog
genProg = Prog <$> resize 3 (listOf genFun)


genFun :: Gen Fun
genFun = do
    funType  <- genType
    funName  <- genName
    funArgs  <- resize 3 (listOf genName)
    numStats <- choose (0, 3)
    funStats <- (genStat funArgs numStats [])
    return (Fun funType funName funArgs funStats)

genStat :: [String] -> Int ->  [Stat] -> Gen [Stat]
genStat args 0 stats = pure stats
genStat args numStats stats = 
    do 
        stat <- frequency[(1, genIFT), (10, genAssign),(10, genDecl), (1, genWhile)]
        if (fst(getName stat))
            then genStat (args++(snd(getName stat))) (numStats-1) (stats ++ [stat])
            else genStat args (numStats-1) (stats ++ [stat])
    where
        getName (Decl _ name) = (True, [name])
        getName _ = (False, [] )
        genIFT = do
            iftCond       <- genExp args
            numStatsTrue  <- choose (0, 3)
            numStatsFalse <- choose (0, 3)
            iftStatsTrue  <- (genStat args numStatsTrue [])
            iftStatsFalse <- (genStat args numStatsFalse [])
            return (IFT iftCond iftStatsTrue iftStatsFalse)
        genAssign = do
            if (args == []) 
                then do decl <- genDecl
                        return decl
                else do name <- elements args
                        exp <- genExp args
                        return (Assign name exp)
        genDecl = Decl <$> genType <*> genName
        genWhile = do
            whCond   <- genExp args
            numStats <- choose (0, 3)
            whStats  <- (genStat args numStats [])
            return (While whCond whStats)


genType :: Gen Type
genType = elements [IntDenotation, CharDenotation]



genExp :: [String] -> Gen Exp
genExp args = frequency[(1, genAdd), (1, genMul),(1, genOr),
                   (1, genAnd), (1, genNot),  (1, genGT),
                   (1, genLT), (1, genEq), (1, genDif), 
                   (10, genVar), (10, genConst), (1, genFun)
                   ]
    where
        genAdd = Add <$> (genExp args) <*> (genExp args) 
        genMul = Mul <$> (genExp args) <*> (genExp args) 
        genOr = OR <$> (genExp args) <*> (genExp args) 
        genAnd = AND <$> (genExp args) <*> (genExp args) 
        genNot = NOT <$> (genExp args)
        genGT = GT <$> (genExp args) <*> (genExp args) 
        genLT = LT <$> (genExp args) <*> (genExp args) 
        genEq = EQU <$> (genExp args) <*> (genExp args) 
        genDif = DIF <$> (genExp args) <*> (genExp args) 
        genVar = do
            if (args == []) 
                then do const <- genConst
                        return const
                else do name <- elements args
                        return (Var name)
        genConst = Const <$> genNum
        genFun =  FunCall <$> genName <*> resize 3 (listOf (genExp args))
        

