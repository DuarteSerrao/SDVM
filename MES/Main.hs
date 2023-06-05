--------------------------------------------------------------------------------
-- Universidade do Minho
-- Mestrado em Engenharia Informática
-- Perfil SDVM - Manutenção e Evolução de Software
-- ALUNOS: pg50289 - Catarina Martins
--         a83630  - Duarte Serrão
--         pg46542 - Pedro Melo
--------------------------------------------------------------------------------
module Main where

import Properties
import Test.QuickCheck.Gen(sample)
import Test.QuickCheck


main :: IO ()
main = quickCheck prop_parser
