import Test.HUnit
import Text.ParserCombinators.Parsec

type Var = String
data IntExp
    = IVar Var
    | ICon Int
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

data Decl
    = Var
    deriving (Eq, Read, Show)

data Stmt
    = Begin [Decl] [Stmt]
    | Assign Var IntExp
    | Read Var
    | Write IntExp
    | IfThenElse BoolExp Stmt Stmt
    | While BoolExp Stmt
    deriving (Eq, Read, Show)

main = do 
    runTestTT expr_testcases

expr_testcases =
    TestList (map mktestcase expr_tests)
    where mktestcase (str, expect) =
            TestCase (case parse_expr str of
                         Left err -> assertFailure (show err)
                         Right parsed -> assertEqual str expect parsed)

expr_tests = [
        ("1", ICon 1),
        ("1+2", Add (ICon 1) (ICon 2)),
        ("1-2", Sub (ICon 1) (ICon 2)),
        ("-1",  Sub (ICon 0) (ICon 1)),
        ("1*2", Mul (ICon 1) (ICon 2)),
        ("1/2", Div (ICon 1) (ICon 2)),
        ("1/0", Div (ICon 1) (ICon 0))
    ]

parse_expr :: String -> Either ParseError IntExp
parse_expr str = parse expr "(unknown)" str

expr :: GenParser Char st IntExp
expr =
    do result <- number
       let num = read result :: Int
       return (ICon num)

number :: GenParser Char st String
number =
    many (oneOf ['0'..'9'])


prog9 = unlines [
    "begin[a,b,c]",
    "    read i;",
    "    n := 1;",
    "    while i>0 do",
    "        begin",
    "            n = 2*n;",
    "            i := i-1",
    "        end",
    "    write n",
    "end"]
