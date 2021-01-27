module Main where

import System.Environment ( getArgs )

main :: IO ()
main = do
    args <- getArgs
    case args of 
        [file] -> do
            x <- readFile file
            putStr x
        _ -> putStrLn "Wrong number of arguments, should provide one file."