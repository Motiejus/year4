import System.IO
import Control.Monad
import Test.HUnit
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

import qualified Text.ParserCombinators.Parsec.Token as Token

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
    = LT IntExp IntExp
    | EQ IntExp IntExp
    | GT IntExp IntExp
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
                                       "end"
                                     ],
             Token.reservedOpNames = ["+", "-", "*", "/", ":=",
                                      "<", ">", "and", "or", "not"]
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
--  <|> readStmt
--  <|> writeStmt
--  <|> ifStmt

beginStmt :: Parser Stmt
beginStmt = 
    do reserved "begin"
       list <- (sepBy1 statement semi)
       reserved "end"
       return $ Begin list

assignStmt :: Parser Stmt
assignStmt =
  do var  <- identifier
     reservedOp ":="
     expr <- aExpression
     return $ Assign var expr

aExpression :: Parser IntExp
aExpression = buildExpressionParser aOperators aTerm

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
