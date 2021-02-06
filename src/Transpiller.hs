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
transpileBlock block level = Right (identForLevel level ++ "pass\n")

transpileStatement :: Statement -> Int -> Either String String
transpileStatement (Assign name value) level = errorOrValue (transpileAssignStatement name value level)
transpileStatement (Return value) level = errorOrValue (transpileReturnStatement value level)
transpileStatement (If condition block) level = errorOrValue (transpileIfStatement condition block level)
transpileStatement (IfElse condition ifBlock elseBlock) level = errorOrValue (transpileIfElseStatement condition ifBlock elseBlock level)
transpileStatement (While condition block) level = errorOrValue (transpileWhileStatement condition block level)
transpileStatement (PrintFunc text) level = errorOrValue (transpilePrintStatement text level)
transpileStatement (FuncCall name args) level = errorOrValue (transpileFuncCallStatement name args level)

-- TODO implement
transpileAssignStatement :: String -> ValueExp -> Int -> Either String String
transpileAssignStatement name value level =  Right (identForLevel level ++ "# Not implemented\n")

-- TODO implement
transpileReturnStatement :: ValueExp -> Int -> Either String String
transpileReturnStatement value level =  Right (identForLevel level ++ "return 0\n") -- TODO transpile valueExp

-- TODO add extra condition to optimize when if condition is constant
transpileIfStatement :: BoolExp  -> Block -> Int -> Either String String
transpileIfStatement condition block level 
    | hasError conditionResult = errorOrValue conditionResult
    | otherwise = errorOrPrepend (transpileBlock block (level + 1)) 
                                    (identForLevel level ++ "if " ++ unwrap conditionResult ++ ":\n")
    where conditionResult = transpileBoolExp condition

-- TODO add extra condition to optimize when if condition is constant
transpileIfElseStatement :: BoolExp  -> Block -> Block -> Int -> Either String String
transpileIfElseStatement condition ifBlock elseBlock level 
    | hasError ifResult = errorOrValue ifResult
    | otherwise = errorOrPrepend (transpileBlock elseBlock (level + 1)) (unwrap ifResult ++ identForLevel level ++ "else:\n")
    where ifResult = transpileIfStatement condition ifBlock level

transpileWhileStatement :: BoolExp  -> Block -> Int -> Either String String
transpileWhileStatement condition block level 
    | hasError conditionResult = errorOrValue conditionResult
    | otherwise = errorOrPrepend (transpileBlock block (level + 1)) 
                                    (identForLevel level ++ "while " ++ unwrap conditionResult ++ ":\n")
    where conditionResult = transpileBoolExp condition

transpilePrintStatement :: StringExp -> Int -> Either String String
transpilePrintStatement text level = errorOr (transpileStringExp text) (identForLevel level ++ "print(") ")"

-- TODO implement
transpileFuncCallStatement :: String -> [ValueExp] -> Int -> Either String String 
transpileFuncCallStatement name args level = Right (identForLevel level ++ "# Not implemented\n")

-- TODO implement
transpileBoolExp :: BoolExp -> Either String String 
transpileBoolExp boolExp = Right "True"

-- TODO implement
transpileStringExp :: StringExp -> Either String String 
transpileStringExp stringExp = Right "# Not implemented"