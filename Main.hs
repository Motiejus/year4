import Prelude hiding (lookup)

import System.IO
import System.Environment
import Control.Monad
import Test.HUnit
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

import qualified Data.Map as Map

import qualified Text.ParserCombinators.Parsec.Token as Token

--------------------------------------------------------------------------------
-- Abstract Syntax Tree
--------------------------------------------------------------------------------

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
    = Yes
    | No
    | Not BoolExp
    | Or BoolExp BoolExp
    | And BoolExp BoolExp
    | Less IntExp IntExp
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

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

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
     cond <- boolExpression
     reserved "then"
     stmt1 <- statement
     reserved "else"
     stmt2 <- statement
     return $ IfThenElse cond stmt1 stmt2

whileStmt :: Parser Stmt
whileStmt =
  do reserved "while"
     cond <- boolExpression
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

boolExpression =   (reservedOp "true" >> return Yes)
               <|> (reservedOp "false" >> return No)
               <|> (do reservedOp "not"
                       exp <- boolExpression
                       return $ Not exp)
               <|> relExpression
relExpression =
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
        [
            Infix (reservedOp "*" >> return Mul) AssocLeft,
            Infix (reservedOp "/" >> return Div) AssocLeft
        ],
        [
            Infix (reservedOp "+" >> return Add) AssocLeft,
            Infix (reservedOp "-" >> return Sub) AssocLeft
        ]
    ]

parseString :: String -> Stmt
parseString str =
  case parse whileParser "" str of
    Left e  -> error $ show e
    Right r -> r

--------------------------------------------------------------------------------
-- Interpreter
--------------------------------------------------------------------------------

type Env = Map.Map Var Integer

update :: Var -> Integer -> Env -> Env
update var val env = Map.insert var val env

lookup :: Var -> Env -> Integer
lookup var env = case Map.lookup var env of Just x -> x

eval :: Env -> Stmt -> IO Env
eval env (Begin ss) =
  do foldM eval env ss
eval env (Assign var exp) =
  let newval = reduce_e env exp
  in return (update var newval env)
eval env (Read var) =
  do val <- getLine
     let int = read val :: Integer
        in return (update var int env)
eval env (Write var) =
  do putStr . show $ reduce_e env var
     putStr " "
     return (env)
eval env (IfThenElse bool stmt1 stmt2) =
  do eval env (if reduce_b env bool
                 then stmt1
                 else stmt2)
eval env (While bool stmt) =
  do if (reduce_b env bool)
     then do env' <- eval env stmt
             eval env' (While bool stmt)
     else return env

reduce_e :: Env -> IntExp -> Integer
reduce_e env (ICon int) = int
reduce_e env (IVar var) = lookup var env
reduce_e env (Add a b) = reduce_e env a + reduce_e env b
reduce_e env (Sub a b) = reduce_e env a - reduce_e env b
reduce_e env (Mul a b) = reduce_e env a * reduce_e env b
reduce_e env (Div a b) = reduce_e env a `div` reduce_e env b

reduce_b :: Env -> BoolExp -> Bool
reduce_b env Yes = True
reduce_b env No = False
reduce_b env (Not stmt) = not $ reduce_b env stmt
reduce_b env (Less stmt1 stmt2) = reduce_e env stmt1 < reduce_e env stmt2
reduce_b env (Equal stmt1 stmt2) = reduce_e env stmt1 == reduce_e env stmt2
reduce_b env (Greater stmt1 stmt2) = reduce_e env stmt1 > reduce_e env stmt2

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] ->
            do
                prog <- getProgName
                putStrLn $ "Usage: " ++ prog ++ " program.alg"
                putStrLn "... running tests ..."
                runTestTT expr_testcases
                return ()
        (filename:[]) ->
            do handle <- openFile filename ReadMode
               prog <- hGetContents handle
               eval (Map.fromList []) (parseString prog)
               hClose handle
               return ()


--------------------------------------------------------------------------------
-- Test
--------------------------------------------------------------------------------

expr_testcases =
     TestList (map mktestcase expr_tests)
     where mktestcase (str, expect) =
             let res = parseString str
             in TestCase (assertEqual str expect res)

expr_tests =
    let a = IVar "a"
        b = IVar "b"
        c = IVar "c"
    in [
        ("write 1", Write $ ICon 1),
        ("write 1+2", Write $ Add (ICon 1) (ICon 2)),
        ("write 1-2", Write $ Sub (ICon 1) (ICon 2)),
        ("write 1*2", Write $ Mul (ICon 1) (ICon 2)),
        ("write 1/2", Write $ Div (ICon 1) (ICon 2)),

        ("write a",       Write a),
        ("write a+b",     Write $ Add a b),
        ("write a+b+c",   Write $ Add (Add a b) c),
        ("write a+(b*c)", Write $ Add a (Mul b c)),
        ("write a+b*c",   Write $ Add a (Mul b c)),

        ("while true do write a", While Yes (Write a)),
        ("while a<b do write a", While (Less a b) (Write a)),
        ("while not a<b do write a", While (Not (Less a b)) (Write a)),
        ("while not true do write a", While (Not Yes) (Write a))
        ]
