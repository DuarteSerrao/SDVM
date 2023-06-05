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

instance Arbitrary Prog where
        arbitrary = genProg

alphabet :: String
alphabet = ['a' .. 'z']


genName :: Gen String
genName = listOf1 (elements alphabet)

genNum :: Gen Int
genNum = arbitrary


genProg :: Gen Prog
genProg = Prog <$> resize 3 (listOf genFun)


genFun :: Gen Fun
genFun = Fun <$> genType <*> genName <*> resize 3 (listOf genName) <*> resize 3 (listOf genStat)

genStat :: Gen Stat
genStat = frequency[(1, genIFT), (10, genAssign),(10, genDecl), (1, genWhile)]
    where
        genIFT = IFT <$> genExp <*> resize 3 (listOf genStat) <*> resize 3 (listOf genStat)
        genAssign = Assign <$> genName <*> genExp
        genDecl = Decl <$> genType <*> genName
        genWhile = While <$> genExp <*> resize 3 (listOf genStat)


genType :: Gen Type
genType = elements [IntDenotation, CharDenotation]



genExp :: Gen Exp
genExp = frequency[(1, genAdd), (1, genMul),(1, genOr),
                   (1, genAnd), (1, genNot),  (1, genGT),
                   (1, genLT), (1, genEq), (1, genDif), 
                   (10, genVar), (10, genConst), (1, genFun)
                   --(1, genPar)
                   ]
    where
        genAdd = Add <$> genExp <*> genExp 
        genMul = Mul <$> genExp <*> genExp 
        genOr = OR <$> genExp <*> genExp 
        genAnd = AND <$> genExp <*> genExp 
        genNot = NOT <$> genExp
        genGT = GT <$> genExp <*> genExp 
        genLT = LT <$> genExp <*> genExp 
        genEq = EQU <$> genExp <*> genExp 
        genDif = DIF <$> genExp <*> genExp 
        genVar = Var <$> genName
        genConst = Const <$> genNum
        genFun =  FunCall <$> genName <*> resize 3 (listOf genExp)
        

