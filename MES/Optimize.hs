--------------------------------------------------------------------------------
-- Universidade do Minho
-- Mestrado em Engenharia Informática
-- Perfil SDVM - Manutenção e Evolução de Software
-- ALUNOS: pg50289 - Catarina Martins
--         a83630  - Duarte Serrão
--         pg46542 - Pedro Melo
--------------------------------------------------------------------------------
module Optimize (optProgTD, optProgBU, optProgI) where

import CDP
import Library.Ztrategic
import Library.StrategicData (StrategicData)
import Data.Generics.Zipper
import Data.Maybe
import Prelude hiding (GT, LT)

instance StrategicData Prog
instance StrategicData Fun
instance StrategicData Stat
instance StrategicData Type
instance StrategicData Exp


optProgTD :: Prog -> Prog
optProgTD prog = fromZipper (fromJust (applyTP (full_tdTP step) (toZipper prog)))
    where step = idTP `adhocTP` optExp

optProgBU :: Prog -> Prog
optProgBU prog = fromZipper (fromJust (applyTP (full_buTP step) (toZipper prog)))
    where step = idTP `adhocTP` optExp

optProgI :: Prog -> Prog
optProgI prog = fromZipper (fromJust (applyTP (innermost step) (toZipper prog)))
    where step = failTP `adhocTP` optExpI


optExp :: Exp -> Maybe Exp
optExp (Add (Const 0) exp) = Just exp                       -- 0 + x = x
optExp (Add exp (Const 0)) = Just exp
optExp (Add (Const x) (Const y)) = Just (Const (x + y))
optExp (Mul (Const 1) exp) = Just exp                       -- 0 * x = 0
optExp (Mul exp (Const 1)) = Just exp                      
optExp (Mul (Const 0) exp) = Just (Const 0)                 -- 1 * x = x
optExp (Mul exp (Const 0)) = Just (Const 0)
optExp (Mul (Const x) (Const y)) = Just (Const (x * y))
optExp (OR (Const 0) exp) = Just exp                        -- 0 || exp -> exp
optExp (OR exp (Const 0)) = Just exp
optExp (OR (Const x) exp) = Just (Const 1)                  -- x || exp -> x
optExp (OR exp (Const x)) = Just (Const 1)
optExp (AND (Const 0) exp) = Just (Const 0)                 -- 0 && exp -> 0
optExp (AND exp (Const 0)) = Just (Const 0)
optExp (AND (Const x) exp) = Just exp                       -- x && exp -> exp
optExp (AND exp (Const x)) = Just exp
optExp (NOT (Const 0)) = Just (Const 1)                     -- Se forem constantes, pode-se ver logo
optExp (NOT (Const x)) = Just (Const 0)
optExp (GT exp1 exp2) 
    | exp1 == exp2 = Just (Const 0)                         -- Se forem completamente iguais, falso
optExp (GT (Const x) (Const y))                             -- Se forem constantes, pode-se ver logo
    | x > y = Just (Const 1)
    | x < y = Just (Const 0)
optExp (LT exp1 exp2) 
    | exp1 == exp2 = Just (Const 0)                         -- Se forem completamente iguais, falso
optExp (LT (Const x) (Const y))                             -- Se forem constantes, pode-se ver logo
    | x > y = Just (Const 0)
    | x < y = Just (Const 1)
optExp (EQU exp1 exp2) 
    | exp1 == exp2 = Just (Const 1)                         -- Se forem completamente iguais, verdadeiro
optExp (DIF exp1 exp2) 
    | exp1 == exp2 = Just (Const 0)                         -- Se forem completamente iguais, falso
optExp exp = Just exp


optExpI :: Exp -> Maybe Exp
optExpI (Add (Const 0) exp) = Just exp
optExpI (Add exp (Const 0)) = Just exp
optExpI (Add (Const x) (Const y)) = Just (Const (x + y))
optExpI (Mul (Const 1) exp) = Just exp
optExpI (Mul exp (Const 1)) = Just exp
optExpI (Mul (Const 0) exp) = Just (Const 0)
optExpI (Mul exp (Const 0)) = Just (Const 0)
optExpI (Mul (Const x) (Const y)) = Just (Const (x * y))
optExpI (OR (Const 0) exp) = Just exp
optExpI (OR exp (Const 0)) = Just exp
optExpI (OR (Const x) exp) = Just (Const 1)
optExpI (OR exp (Const x)) = Just (Const 1)
optExpI (AND (Const 0) exp) = Just (Const 0)
optExpI (AND exp (Const 0)) = Just (Const 0)
optExpI (AND (Const x) exp) = Just exp
optExpI (AND exp (Const x)) = Just exp
optExpI (NOT (Const 0)) = Just (Const 1)
optExpI (NOT (Const x)) = Just (Const 0)
optExpI (GT exp1 exp2) 
    | exp1 == exp2 = Just (Const 0)
optExpI (GT (Const x) (Const y))
    | x > y = Just (Const 1)
    | x < y = Just (Const 0)
optExpI (LT exp1 exp2) 
    | exp1 == exp2 = Just (Const 0)
optExpI (LT (Const x) (Const y))
    | x > y = Just (Const 0)
    | x < y = Just (Const 1)
optExpI (EQU exp1 exp2) 
    | exp1 == exp2 = Just (Const 1)
optExpI (DIF exp1 exp2) 
    | exp1 == exp2 = Just (Const 0)
optExpI _ = Nothing




