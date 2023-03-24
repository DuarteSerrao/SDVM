--------------------------------------------------------------------------------
-- Universidade do Minho
-- Mestrado em Engenharia Informática
-- Perfil SDVM - Manutenção e Evolução de Software
-- ALUNOS: pg50289 - Catarina Martins
--         a83630  - Duarte Serrão
--         pg46542 - Pedro Melo
--------------------------------------------------------------------------------
import Prelude hiding (GT, LT)
import Text.Parsec
import Text.Parsec.String (Parser)
import Data.List
-- A minha ling é constituída por uma lista de funções

data Prog = Prog [Fun]
        deriving (Show)


-- uma função é constutída por uma lista de statements
--  (falta  a lista de parametros formais)


data Fun = Fun Type String [String] [Stat]
        deriving (Show)


-- considero os seguintes stats:
--  if then else
-- attrinutição
-- decl
-- loop - while

data Stat = IFT Exp  [Stat] [Stat]
        | Assign String Exp
        | Decl   Type String -- Exp     --- int a = 33;
        | While  Exp [Stat]
        deriving (Show)

-- só considero dois tipos
data Type = IntDenotation
          | CharDenotation
          deriving (Show)

-- as expressões são:
data Exp = Add Exp Exp
         | Mul Exp Exp
         | Or  Exp Exp
         | GT  Exp Exp
         | LT  Exp Exp
         | Var String
         | Const String
         | FunCall String [Exp]    --  f (2+3,"aa")
         deriving (Show)



-- temos de fazer um parser

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
                   try(Or <$ string "||"), 
                   try(GT <$ char '>'),
                   try(GT <$ char '<')]
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
constParser = Const <$> (spaces *> many1 digit <* spaces)

parensExpParser :: Parser Exp
parensExpParser = char '(' *> spaces *> expParser <* spaces <* char ')' <* spaces
-- e para o validarmos, escrever um unparser ??

unparser :: Prog -> String
unparser = unparseProg

unparseProg :: Prog -> String
unparseProg (Prog funs) = unlines (map unparseFun funs)

unparseArgs :: [String] -> String
unparseArgs [] = " "
unparseArgs [x] = x
unparseArgs (h:t) = h ++ ", " ++ unparseArgs t


unparseFun :: Fun -> String
unparseFun (Fun tp name args stats) = unparseType(tp) ++ " " ++ name ++ " ("++ unparseArgs args  ++ ") {"++ unlines (map unparseStat stats) ++ "}"

unparseStat :: Stat -> String
unparseStat (IFT cond thens elses) =
  "if (" ++ unparseExp cond ++ ") { " ++
  unlines (map ("  " ++) (map unparseStat thens)) ++
  "} else { " ++
  unlines (map ("  " ++) (map unparseStat elses)) ++
  "}"
unparseStat (Assign var val) = var ++ " = " ++ unparseExp val ++ ";"
unparseStat (Decl ty var) = unparseType ty ++ " " ++ var ++ ";"
unparseStat (While cond stats) =
  "while (" ++ unparseExp cond ++ ") { " ++
  unlines (map ("  " ++) (map unparseStat stats)) ++
  "}"

unparseType :: Type -> String
unparseType IntDenotation = "int"
unparseType CharDenotation = "char"

unparseExp :: Exp -> String
unparseExp (Add e1 e2) = unparseExp e1 ++ " + " ++ unparseExp e2
unparseExp (Mul e1 e2) = unparseExp e1 ++ " * " ++ unparseExp e2
unparseExp (Or e1 e2) = unparseExp e1 ++ " || " ++ unparseExp e2
unparseExp (GT e1 e2) = unparseExp e1 ++ " > " ++ unparseExp e2
unparseExp (LT e1 e2) = unparseExp e1 ++ " < " ++ unparseExp e2
unparseExp (Var v) = v
unparseExp (Const c) = c
unparseExp (FunCall name args) = name ++ "(" ++ intercalate ", " (map unparseExp args) ++ ")"
-- pretty printing


exampleProg :: String
exampleProg = unlines
  [ "int main( arg ) {"
  , "  int a;"
  , "  a = 0;"
  , "  while (a < 10) {"
  , "    if (a > 5) {"
  , "      a = a + 1;"
  , "    } else {"
  , "      a = (a + 2)*(a+1);"
  , "    }"
  , "  }"
  , "}"
  ]

test1 :: Either ParseError String
test1 = case parser exampleProg of
        Left err -> Left err
        Right tree -> Right (unparser tree)