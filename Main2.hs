import System.IO
import Control.Monad
import Test.HUnit
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

import Data.Map as Map

import qualified Text.ParserCombinators.Parsec.Token as Token

type Env = Map Var Integer

type Var = String
data IntExp
    = IVar Var
    | ICon Integer
    | Add IntExp IntExp
    | Sub IntExp IntExp
    | Mul IntExp IntExp
    | Div IntExp IntExp
    deriving (Eq, Read, Show)

data BoolExp
    = Less IntExp IntExp
    | Equal IntExp IntExp
    | Greater IntExp IntExp
    deriving (Eq, Read, Show)

data Stmt
    = Begin [Stmt]
    | Assign Var IntExp
    | Read Var
    | Write IntExp
    | IfThenElse BoolExp Stmt Stmt
    | While BoolExp Stmt
    deriving (Eq, Read, Show)

languageDef =
  emptyDef { Token.commentStart    = "/*",
             Token.commentEnd      = "*/",
             Token.commentLine     = "//",
             Token.identStart      = letter,
             Token.identLetter     = alphaNum,
             Token.reservedNames   = [ "if",
                                       "then",
                                       "else",
                                       "while",
                                       "do",
                                       "skip",
                                       "true",
                                       "false",
                                       "not",
                                       "and",
                                       "or",
                                       "begin",
                                       "end",
                                       "read",
                                       "write"
                                     ],
             Token.reservedOpNames = ["+", "-", "*", "/", ":=",
                                      "=", "<", ">", "and", "or", "not"]
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
braces     = Token.braces     lexer -- passes surrounding braces
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace

whileParser :: Parser Stmt
whileParser = whiteSpace >> statement

statement :: Parser Stmt
statement =
        beginStmt
    <|> assignStmt
    <|> readStmt
    <|> writeStmt
    <|> ifStmt
    <|> whileStmt

beginStmt :: Parser Stmt
beginStmt = 
    do reserved "begin"
       list <- (sepBy1 statement semi)
       reserved "end"
       return $ Begin list

ifStmt :: Parser Stmt
ifStmt =
  do reserved "if"
     cond <- rExpression
     reserved "then"
     stmt1 <- statement
     reserved "else"
     stmt2 <- statement
     return $ IfThenElse cond stmt1 stmt2

whileStmt :: Parser Stmt
whileStmt =
  do reserved "while"
     cond <- rExpression
     reserved "do"
     stmt <- statement
     return $ While cond stmt

assignStmt :: Parser Stmt
assignStmt =
  do var  <- identifier
     reservedOp ":="
     expr <- aExpression
     return $ Assign var expr

readStmt :: Parser Stmt
readStmt =
  do reserved "read"
     var <- identifier
     return $ Read var

writeStmt :: Parser Stmt
writeStmt =
  do reserved "write"
     var <- aExpression
     return $ Write var

aExpression :: Parser IntExp
aExpression = buildExpressionParser aOperators aTerm

rExpression =
  do a1 <- aExpression
     op <- relation
     a2 <- aExpression
     return $ op a1 a2

relation =   (reservedOp ">" >> return Greater)
         <|> (reservedOp "=" >> return Equal)
         <|> (reservedOp "<" >> return Less)

aTerm =  parens aExpression
     <|> liftM IVar identifier
     <|> liftM ICon integer

aOperators = [
    [Infix  (reservedOp "+"   >> return Add) AssocLeft],
    [Infix  (reservedOp "-"   >> return Sub) AssocLeft],
    [Infix  (reservedOp "*"   >> return Mul) AssocLeft],
    [Infix  (reservedOp "/"   >> return Div) AssocLeft]
    ]

parseString :: String -> Stmt
parseString str =
  case parse whileParser "" str of
    Left e  -> error $ show e
    Right r -> r

--------------------------------------------------------------------------------
-- Interpreter
--------------------------------------------------------------------------------

update :: Var -> Integer -> Env -> Env
update var val env =
    Map.insert var val env
lookup ::  Var -> Env -> Maybe Integer
lookup var env =
    Map.lookup var env

interpret :: Env -> Stmt -> IO ()
interpret env (Write x) =
    putStrLn $ write env x

write :: Env -> IntExp -> String
write env (ICon int) =
    show int
