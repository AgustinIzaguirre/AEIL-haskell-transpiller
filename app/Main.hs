module Main where

import System.Environment ( getArgs )
import Parser
import AST (Program, Block, Statement)
import Text.Parsec
import Transpiller

main :: IO ()
main = do
    args <- getArgs
    case args of 
        [file] -> do
            code <- readFile file
            result <- compile code
            print result
        _ -> putStrLn "Wrong number of arguments, should provide one file."

compile :: String -> IO ()
compile code = do 
    case parse (parseFile <* eof)  "" code of
        Left error  -> print error >> fail "parse error"
        Right result -> case transpileProgram result of
                        Nothing -> fail "Transpile error"
                        Just code -> writeFile "output.py" code >> print code
