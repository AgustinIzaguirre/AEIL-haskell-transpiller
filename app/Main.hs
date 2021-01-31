module Main where

import System.Environment ( getArgs )
import Parser
import AST (Program, Block, Statement)
import Text.Parsec

main :: IO ()
main = do
    args <- getArgs
    case args of 
        [file] -> do
            code <- readFile file
            result <- compile code
            print result
        _ -> putStrLn "Wrong number of arguments, should provide one file."

compile :: String -> IO Program
compile code = do 
    case parse parseFile  "" code of
        Left error  -> print error >> fail "parse error"
        Right result -> return result