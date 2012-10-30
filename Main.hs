
import Test.HUnit

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

data Stmt
    = Begin [Stmt]
    | Assign Var IntExp
    | Read Var
    | Write IntExp
    | IfThenElse BoolExp Stmt Stmt
    | While BoolExp Stmt
    deriving (Eq, Read, Show)

main = do 
    runTestTT parse_testcases


parse_testcases = TestList [
        TestLabel expr (TestCase (assertEqual expr (parse_ast expr) expect)) |
        (expr, expect) <- parse_tests]

parse_tests = [
        ("1", ICon 1),
        ("-1", ICon (-1)),
        ("1+2", Add (ICon 1) (ICon 2)),
        ("1-2", Sub (ICon 1) (ICon 2)),
        ("1*2", Mul (ICon 1) (ICon 2)),
        ("1/2", Div (ICon 1) (ICon 2))
    ]

parse_ast = undefined


prog9 = unlines [
    "begin",
    "    read i;",
    "    n := 1;",
    "    while i>0 do",
    "        begin",
    "            n = 2*n;",
    "            i := i-1",
    "        end",
    "    write n",
    "end"]
