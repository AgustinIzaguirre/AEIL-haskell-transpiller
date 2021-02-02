module Transpiller where

import AST
import Lib
import Data.List

-- Probabbly make a function that returns [(FuncName, ([Parameters, FuncAST))] from Program
-- hasMain $ map fst pairTest where pairTest is the array of [(FuncName, ([Parameters, FuncAST))]
-- parameters to check if quantity matches on every call, funcName to check for main and to check function exists

-- TODO replace Maybe with Either
transpileProgram :: Program -> Maybe String
transpileProgram program 
                | hasMainFunction programFunctions = Just (concat (fmap transpileFunction programFunctions) ++ "\nmain()")
                | otherwise = Nothing
                where programFunctions = getProgramFunctions program

transpileFunction :: FunctionData -> String
transpileFunction funcData = "def " ++ fst funcData ++ "(" ++ 
                                transpileFuncParameters ((fst . snd) funcData) ++ "):\n" ++ 
                                transpileBlock ((getFunctionBlock . snd . snd) funcData) 1 ++ "\n"

transpileBlock :: Block -> Int -> String 
transpileBlock block level = identForLevel 1 ++ "pass\n"

transpileFuncParameters :: [String] -> String
transpileFuncParameters = intercalate ", " 
