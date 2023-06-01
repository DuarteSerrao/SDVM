--------------------------------------------------------------------------------
-- Universidade do Minho
-- Mestrado em Engenharia Informática
-- Perfil SDVM - Manutenção e Evolução de Software
-- ALUNOS: pg50289 - Catarina Martins
--         a83630  - Duarte Serrão
--         pg46542 - Pedro Melo
--------------------------------------------------------------------------------
module CDP (parser, unparser, Prog(Prog), Fun(Fun), Stat(IFT, Assign, Decl, While),
            Type(IntDenotation, CharDenotation), 
            Exp(Add, Mul, OR, AND, NOT, GT, LT, EQU, DIF, Var, Const, FunCall, PAR)) where
        
import Prelude hiding (GT, LT)
import Text.Parsec
import Text.Parsec.String (Parser)
import Data.List

-- Um programa é constituido por um conjunto de funções
data Prog = Prog [Fun]
        deriving (Show)


-- Uma função é constítuida pelo tipo, nome, lista de argumentos e lista de statments
data Fun = Fun Type String [String] [Stat]
        deriving (Show)


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
          deriving (Show)

-- Tipos podem ser:
    -- Int
    -- Char
data Type = IntDenotation
          | CharDenotation
          deriving (Show)

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
         | PAR Exp
         deriving (Show)




parser :: String -> Either ParseError Prog
parser = parse progParser ""

progParser :: Parser Prog
progParser = Prog <$> many funParser

argsParser :: Parser [String]
argsParser = sepBy1 idParser (char ',' *> spaces)

funParser :: Parser Fun
funParser = Fun <$> (typeParser <* spaces)
                 <*> idParser
                 <*> (skipMany1 (char '(') *> spaces *> argsParser <* char ')' <* spaces <* char '{' <* spaces)
                 <*> (many statParser)
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
expParser = chainl1 atomParser opParser

opParser :: Parser (Exp -> Exp -> Exp)
opParser = spaces *>
           choice [try (Add <$ char '+'),
                   try (Mul <$ char '*'), 
                   try(OR   <$ string "||"), 
                   try(AND  <$ string "&&"), 
                   try(GT   <$ char '>'),
                   try(LT   <$ char '<'),
                   try(EQU  <$ string "=="), 
                   try(DIF  <$ string "!=")]
                   
           <* optional (char '=')
           <* spaces

atomParser :: Parser Exp
atomParser = choice [try funCallParser, try varParser, try constParser, try parensExpParser]

funCallParser :: Parser Exp
funCallParser = FunCall <$> (spaces *> many1 letter <* spaces <* char '(' <* spaces)
                         <*> sepBy expParser (char ',' <* spaces) <* char ')' <* spaces

idParser :: Parser String
idParser = spaces *> many1 (letter) <* spaces

varParser :: Parser Exp
varParser = Var <$> idParser

constParser :: Parser Exp
constParser = Const <$> (spaces *> (read <$> (many1 digit)) <* spaces)

parensExpParser :: Parser Exp
parensExpParser = PAR <$> (char '(' *> spaces *> expParser <* spaces <* char ')' <* spaces)


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
unparseExp (Add e1 e2) = unparseExp e1 ++ " + " ++ unparseExp e2
unparseExp (Mul e1 e2) = unparseExp e1 ++ " * " ++ unparseExp e2
unparseExp (OR e1 e2) = unparseExp e1 ++ " || " ++ unparseExp e2
unparseExp (AND e1 e2) = unparseExp e1 ++ " || " ++ unparseExp e2
unparseExp (NOT e) = "! " ++ unparseExp e
unparseExp (GT e1 e2) = unparseExp e1 ++ " > " ++ unparseExp e2
unparseExp (LT e1 e2) = unparseExp e1 ++ " < " ++ unparseExp e2
unparseExp (EQU e1 e2) = unparseExp e1 ++ " == " ++ unparseExp e2
unparseExp (DIF e1 e2) = unparseExp e1 ++ " != " ++ unparseExp e2
unparseExp (Var v) = v
unparseExp (Const c) = show c
unparseExp (FunCall name args) = name ++ "(" ++ intercalate ", " (map unparseExp args) ++ ")"
unparseExp (PAR e) = "( " ++ unparseExp e ++ " )"
-- pretty printing


exampleProg :: String
exampleProg = unlines
  [ "int main( arg ) {"
  , "  int a;"
  , "  a = 0;"
  , "  while (a < 10) {"
  , "    if (a > 5) {"
  , "      a = a + 1 * a;"
  , "    } else {"
  , "      a = (a + 2)*(a+1);"
  , "    }"
  , "  }"
  , "}"
  ]

testParser :: Either ParseError Prog
testParser = parser exampleProg

testUnparser :: Either ParseError String
testUnparser = case parser exampleProg of
        Left err -> Left err
        Right tree -> Right (unparser tree)