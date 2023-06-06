--------------------------------------------------------------------------------
-- Universidade do Minho
-- Mestrado em Engenharia Informática
-- Perfil SDVM - Manutenção e Evolução de Software
-- ALUNOS: pg50289 - Catarina Martins
--         a83630  - Duarte Serrão
--         pg46542 - Pedro Melo
--------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
module CDP (parser, unparser, Prog(Prog), Fun(Fun), Stat(IFT, Assign, Decl, While),
            Type(IntDenotation, CharDenotation), 
            Exp(Add, Mul, OR, AND, NOT, GT, LT, EQU, DIF, Var, Const, FunCall), exampleProg) where
        
import Prelude hiding (GT, LT)
import Text.Parsec
import Text.Parsec.String (Parser)
import Data.List
import Data.Data

-- Um programa é constituido por um conjunto de funções
data Prog = Prog [Fun]
        deriving (Show, Eq, Data, Typeable)


-- Uma função é constítuida pelo tipo, nome, lista de argumentos e lista de statments
data Fun = Fun Type String [String] [Stat]
        deriving (Show, Eq, Data, Typeable)


-- Statments podem ser:
    -- If then else
    -- Atribuições
    -- Declarações
    -- Loops:
        -- While
data Stat = IFT Exp  [Stat] [Stat]
          | Assign String Exp
          | Decl   Type String -- Exp     --- int a = 33;
          | While  Exp [Stat]
          deriving (Show, Eq, Data, Typeable)

-- Tipos podem ser:
    -- Int
    -- Char
data Type = IntDenotation
          | CharDenotation
          deriving (Show, Eq, Data, Typeable)

-- Expressões podem ser:
    -- Adição, Multiplicação
    -- OR, AND, NOT
    -- >, <, ==, =/=
    -- Variáveis
    -- Constantes
    -- Chamadas de funções: foo (2+3,"aa")
data Exp = Add Exp Exp
         | Mul Exp Exp
         | OR  Exp Exp
         | AND Exp Exp
         | NOT Exp
         | GT  Exp Exp
         | LT  Exp Exp
         | EQU Exp Exp
         | DIF Exp Exp
         | Var String
         | Const Int
         | FunCall String [Exp] 
         deriving (Show, Eq, Data, Typeable)




parser :: String -> Either ParseError Prog
parser = parse progParser ""

progParser :: Parser Prog
progParser = Prog <$> sepBy funParser  (spaces)

argsParser :: Parser [String]
argsParser = sepBy1 idParser (char ',' *> spaces)

funParser :: Parser Fun
funParser = Fun <$> (typeParser <* spaces)
                 <*> idParser
                 <*> (skipMany1 (char '(') *> spaces *> (option [] argsParser) <* char ')' <* spaces <* char '{' <* spaces)
                 <*> (sepBy statParser (spaces))
                 <* spaces <* char '}'

statParser :: Parser Stat
statParser = choice [try(iftParser), try(declParser), try(assignParser), try whileParser]

typeParser :: Parser Type
typeParser = IntDenotation <$ string "int" <* spaces
         <|> CharDenotation <$ string "char" <* spaces

iftParser :: Parser Stat
iftParser = IFT <$>
            (spaces *> string "if" *> spaces *> char '(' *> spaces *> expParser <* spaces <* char ')' <* spaces)
            <*> (char '{' *> spaces *> many statParser <* spaces <* char '}')
            <*> option [] (spaces *> string "else" *> spaces *> char '{' *> spaces *> many statParser <* spaces <* char '}')

assignParser :: Parser Stat
assignParser = Assign <$> (spaces *> idParser <* spaces <* string "=" <* spaces) <*> expParser <* spaces <* char ';'

declParser :: Parser Stat
declParser = Decl <$> (spaces *> typeParser <* spaces) <*> idParser <* spaces <* char ';'

whileParser :: Parser Stat
whileParser = While <$> (spaces *> string "while" *> spaces *> char '(' *> spaces *> expParser <* spaces <* char ')' <* spaces)
                     <*> (char '{' *> spaces *> many statParser <* spaces <* char '}')



expParser :: Parser Exp
expParser =  choice[try atomParser, try opParser]

opParser :: Parser Exp
opParser = spaces *>
           choice [try addParser,
                   try mulParser, 
                   try orParser, 
                   try andParser, 
                   try gtParser,
                   try ltParser,
                   try eqParser, 
                   try difParser]
                   
           <* optional (char '=')
           <* spaces

firstExpParser :: Parser Exp
firstExpParser = char '(' *> spaces *> expParser <* spaces 

secExpParser :: Parser Exp
secExpParser = expParser <* spaces <* optional (char ')')

addParser :: Parser Exp
addParser = Add <$> firstExpParser <* char '+' <* spaces <*> secExpParser

mulParser :: Parser Exp
mulParser = Mul <$> firstExpParser <* char '*' <* spaces <*> secExpParser

orParser :: Parser Exp
orParser = OR <$> firstExpParser <* string "||" <* spaces <*> secExpParser

andParser :: Parser Exp
andParser = AND <$> firstExpParser <* string "&&" <* spaces <*> secExpParser

eqParser :: Parser Exp
eqParser = EQU <$> firstExpParser <* string "==" <* spaces <*> secExpParser

difParser :: Parser Exp
difParser = DIF <$> firstExpParser <* string "!=" <* spaces <*> secExpParser

gtParser :: Parser Exp
gtParser = GT <$> firstExpParser <* char '>' <* spaces <*> secExpParser

ltParser :: Parser Exp
ltParser = LT <$> firstExpParser <* char '<' <* spaces <*> secExpParser

atomParser :: Parser Exp
atomParser = choice [try funCallParser, try varParser, try constParser, try notParser]--, try parensExpParser

funCallParser :: Parser Exp
funCallParser = FunCall <$> (spaces *> many1 letter <* spaces <* char '(' <* spaces)
                         <*> sepBy expParser (char ',' <* spaces) <* char ')' <* spaces

idParser :: Parser String
idParser = spaces *> many1 (letter) <* spaces

varParser :: Parser Exp
varParser = Var <$> idParser

constParser :: Parser Exp
constParser = Const <$> (spaces *> (read <$> numberParse) <* spaces)

numberParse :: Parser String
numberParse = ((:) <$> (char '-') <*> (many1 digit)) <|>  many1 digit


notParser :: Parser Exp
notParser = NOT <$> (char '!' *> spaces *> expParser <* spaces)


unparser :: Prog -> String
unparser = unparseProg

unparseProg :: Prog -> String
unparseProg (Prog funs) = unwords (map unparseFun funs)

unparseArgs :: [String] -> String
unparseArgs [] = " "
unparseArgs [x] = x
unparseArgs (h:t) = h ++ ", " ++ unparseArgs t


unparseFun :: Fun -> String
unparseFun (Fun tp name args stats) = unparseType(tp) ++ " " ++ name ++ " ("++ unparseArgs args  ++ ") {"++ unwords (map unparseStat stats) ++ "}"

unparseStat :: Stat -> String
unparseStat (IFT cond thens elses) =
  "if (" ++ unparseExp cond ++ ") {" ++
  unwords (map (" " ++) (map unparseStat thens)) ++
  "} else {" ++
  unwords (map (" " ++) (map unparseStat elses)) ++
  "}"
unparseStat (Assign var val) = var ++ " = " ++ unparseExp val ++ ";"
unparseStat (Decl ty var) = unparseType ty ++ " " ++ var ++ ";"
unparseStat (While cond stats) =
  "while (" ++ unparseExp cond ++ ") {" ++
  unwords (map ("  " ++) (map unparseStat stats)) ++
  "}"

unparseType :: Type -> String
unparseType IntDenotation = "int"
unparseType CharDenotation = "char"

unparseExp :: Exp -> String
unparseExp (Add e1 e2) = "(" ++ unparseExp e1 ++ " + " ++ unparseExp e2 ++ ")"
unparseExp (Mul e1 e2) = "(" ++ unparseExp e1 ++ " * " ++ unparseExp e2 ++ ")"
unparseExp (OR e1 e2) = "(" ++ unparseExp e1 ++ " || " ++ unparseExp e2 ++ ")"
unparseExp (AND e1 e2) = "(" ++ unparseExp e1 ++ " && " ++ unparseExp e2 ++ ")"
unparseExp (DIF e1 e2) = "(" ++ unparseExp e1 ++ " != " ++ unparseExp e2 ++ ")"
unparseExp (GT e1 e2) = "(" ++ unparseExp e1 ++ " > " ++ unparseExp e2 ++ ")"
unparseExp (LT e1 e2) = "(" ++ unparseExp e1 ++ " < " ++ unparseExp e2 ++ ")"
unparseExp (EQU e1 e2) = "(" ++ unparseExp e1 ++ " == " ++ unparseExp e2 ++ ")"
unparseExp (NOT e) = "!" ++ unparseExp e
unparseExp (Var v) = v
unparseExp (Const c) = show c
unparseExp (FunCall name args) = name ++ "(" ++ intercalate ", " (map unparseExp args) ++ ")"
-- pretty printing


exampleProg :: String
exampleProg = "int main ( ) {int a; a = 0; while (a < 10) {  if (a > 5) { a = a + 1 * a;} else { a = ( a + 2 ) * ( a + 1 );}}}"

exampleTree = Prog [Fun CharDenotation "sf" [] [],Fun IntDenotation "z" ["zc","w","qb"] [IFT (AND (Var "qk") (FunCall "lbj" [NOT (AND (Mul (Const (-1)) (Var "r")) (Const (-1)))])) [Decl IntDenotation "bc"] [Decl CharDenotation "rye",Decl CharDenotation "th",Assign "mfd" (Add (LT (Const 3) (Const 0)) (Add (Var "nt") (Mul (Var "wz") (DIF (NOT (NOT (FunCall "t" []))) (NOT (Const 2))))))],Assign "aug" (Var "an"),Assign "mjb" (Var "q")]]

exampleTree2 = Prog [Fun CharDenotation "s" ["qt","d"] [IFT (Const (-1)) [Decl IntDenotation "du"] []]]

testParser :: Either ParseError Prog
testParser = parser exampleProg

testUnparser :: Either ParseError String
testUnparser = case parser exampleProg of
        Left err -> Left err
        Right tree -> Right (unparser tree)

testTree = unparser exampleTree

testBoth = parser testTree