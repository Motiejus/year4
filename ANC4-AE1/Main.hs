import System.Environment (getArgs, getProgName)
import System.IO (openFile, hGetContents, hClose, IOMode(ReadMode))

type Node = Integer
type Cost = Integer

data Topology = Topology
    {
        from :: Node,
        to :: Node,
        cost :: Maybe Cost
} deriving (Eq, Read, Show)

main = do
    args <- getArgs
    case args of
        [] ->
            do
                prog <- getProgName
                putStrLn $ "Usage: " ++ prog ++ " topology.txt"
                return ()
        (fn:[]) ->
            do
               handle <- openFile fn ReadMode
               str <- hGetContents handle
               --putStrLn $ "Stuff: " ++ show init
               --init <- readStuff fn
               putStrLn $ show str
               hClose handle
               return ()

readStuff :: String -> Topology
readStuff info =
    Topology {
        from = 0,
        to = 0,
        cost = Just 0
    }
