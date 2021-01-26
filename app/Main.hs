-- module Main where

-- import Lib
-- import Lexer

-- main :: IO ()
-- -- main = parseH
-- main = testFunc 


module Main where
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of 
        [file] -> do
            x <- readFile file
            putStr x
        _ -> putStrLn "Wrong number of arguments, should provide one file."