import Data.List (intercalate)
import System.Environment (getArgs)

processMacros :: String -> (String, [String])
processMacros [] = ([], [])
processMacros ('$':'[':xs) = (p, y : q)
    where (y, z) = span (/= ']') xs
          (p, q) = processMacros (tail z)
processMacros (x:xs) = (x : y, z)
    where (y, z) = processMacros xs

addSources :: (String, [String]) -> String -> String
addSources (x, y) head = x ++ "# " ++ head ++ "\n\n" ++ intercalate "\n" y

transpile :: String -> String -> String
transpile = addSources . processMacros

parseArgs :: [String] -> IO () 
parseArgs [target, result, head] = do
    content <- readFile target
    writeFile result (transpile content head)

parseArgs _ = putStrLn "Error"

main :: IO ()
main = do
    args <- getArgs
    parseArgs args
