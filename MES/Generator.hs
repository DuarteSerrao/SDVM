module Generator(genExp) where

import CDP
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Prelude hiding (GT, LT)

alphabet :: String
alphabet = ['a' .. 'z']



genType :: Gen Type
genType = elements [IntDenotation, CharDenotation]


genName :: Gen String
genName = vectorOf 3 $ elements alphabet

genNum :: Gen String
genNum = resize 3 (listOf $ elements ['0'..'9'])


genExp :: Gen Exp
genExp = frequency[(1, genAdd), (1, genMul),(1, genOr),
                   (1, genAnd), (1, genNot),  (1, genGT),
                   (1, genLT), (1, genEq), (1, genDif), 
                   (10, genVar), (10, genConst), (1, genFun),
                   (1, genPar)]
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
        genPar = PAR <$> genExp
        


--data Exp = Add Exp Exp
--         | Mul Exp Exp
--         | OR  Exp Exp
--         | AND Exp Exp
--         | NOT Exp
--         | GT  Exp Exp
--         | LT  Exp Exp
--         | EQU Exp Exp
--         | DIF Exp Exp
--         | Var String
--         | Const String
--         | FunCall String [Exp]
--         | PAR Exp
--         deriving (Show)