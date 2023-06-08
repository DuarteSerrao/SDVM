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
import Optimize
import CDP
import Mutations
import Generator

main :: IO ()
main = do
    print ("====================================================================")
    print ("Testing Generator")
    print ("--------------------------------------------------------------------")
    progGenerated <- generate genProg
    print (progGenerated)
    print ("Unparsed: " ++ unparser progGenerated)
    print ("====================================================================")
    print ("Testing Mutations")
    print ("--------------------------------------------------------------------")
    print ("Original: " ++ (unparser exampleTree))
    print ("Mutation: " ++ (unparser (mutProg exampleTree)))
    print ("====================================================================")
    print ("Testing Properties")
    print ("--------------------------------------------------------------------")
    print ("Tree equals Tree parsed after being unparsed?")
    quickCheck prop_parser
    print ("Otimization Top-Down equals Down-Up?")
    quickCheck prop_opt_TDvsBU
    print ("Otimization Down-Up equals Innermost?")
    quickCheck prop_opt_BUvsI
    print ("====================================================================")
