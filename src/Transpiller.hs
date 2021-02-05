module Transpiller where

import Data.List

import AST
import Lib
import ErrorMessages

-- Probabbly make a function that returns [(FuncName, ([Parameters, FuncAST))] from Program
-- hasMain $ map fst pairTest where pairTest is the array of [(FuncName, ([Parameters, FuncAST))]
-- parameters to check if quantity matches on every call, funcName to check for main and to check function exists

-- TODO replace Maybe with Either
transpileProgram :: Program -> Either String String
transpileProgram program 
                | hasMainFunction programFunctions = getFunctionsResult programFunctions
                | otherwise = Left noMainFunction
                where programFunctions = getProgramFunctions program

getFunctionsResult :: [FunctionData] -> Either String String
getFunctionsResult functions
    | hasProgramError functionResult = Left (getProgramErrors functionResult) 
    | otherwise = Right (concatMap unwrap functionResult ++ "\nmain()")
    where functionResult = fmap transpileFunction functions

transpileFunction :: FunctionData -> Either String String
transpileFunction funcData
    | hasError blockResult = Left (either id id blockResult)
    | otherwise = Right 
                    (
                        "def " ++ fst funcData ++ "(" ++ 
                        transpileFuncParameters ((fst . snd) funcData) ++ "):\n" ++ 
                        unwrap blockResult ++ "\n"
                    )
    where blockResult = transpileBlock ((getFunctionBlock . snd . snd) funcData) 1

transpileFuncParameters :: [String] -> String
transpileFuncParameters = intercalate ", " 

transpileBlock :: Block -> Int -> Either String String
transpileBlock block level = Right (identForLevel 1 ++ "pass\n")

