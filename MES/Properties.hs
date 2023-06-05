--------------------------------------------------------------------------------
-- Universidade do Minho
-- Mestrado em Engenharia Informática
-- Perfil SDVM - Manutenção e Evolução de Software
-- ALUNOS: pg50289 - Catarina Martins
--         a83630  - Duarte Serrão
--         pg46542 - Pedro Melo
--------------------------------------------------------------------------------
module Properties where

import CDP
import Generator (genProg)

prop_parser ::  Prog -> Bool
prop_parser prog = case parser (unparser prog) of
        Left err -> False
        Right tree -> prog == tree

