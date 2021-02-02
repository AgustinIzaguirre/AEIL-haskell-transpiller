module Transpiller where

import AST

type FunctionData = (String,([String], Function))

-- Probabbly make a function that returns [(FuncName, ([Parameters, FuncAST))] from Program
-- hasMain $ map fst pairTest where pairTest is the array of [(FuncName, ([Parameters, FuncAST))]
-- parameters to check if quantity matches on every call, funcName to check for main and to check function exists

hasMain :: [String] -> Bool
hasMain funcNames = (== 1) . length $ filter (== "main") funcNames

hasMainFunction :: [FunctionData] -> Bool
hasMainFunction functionsData
        | hasMain $ map fst functionsData = maybe False (null . fst . snd) (getFunction functionsData "main")
        | otherwise = False

-- getFunction :: [(String, (Int, Function))] -> Name -> Maybe (String, (Int, Function))
getFunction :: [FunctionData] -> String -> Maybe FunctionData
getFunction functions functionName
    | length functionInfo == 1 = Just (head functionInfo)
    | otherwise = Nothing -- Not declared or redeclared
    where functionInfo = filter ((== functionName) . fst) functions

-- isValidFuncCall :: (String, ([String], Function)) -> [(String, ([String], Function))] -> Bool
isValidFuncCall :: FunctionData -> [FunctionData] -> Bool
isValidFuncCall call functions = case function of
                            Nothing -> False
                            Just f -> (length. fst . snd) call == (length . fst . snd) f 
                        where function = getFunction functions (fst call)

-- TODO replace Maybe with Either
transpileProgram :: Program -> Maybe String
transpileProgram program 
                | hasMainFunction programFunctions = Just (concat (fmap transpileFunction programFunctions))
                | otherwise = Nothing
                where programFunctions = getProgramFunctions program

getProgramFunctions :: Program -> [FunctionData]
getProgramFunctions (Root function) = [getFunctionData function]
getProgramFunctions (Multiple function program) = getFunctionData function : getProgramFunctions program

getFunctionData :: Function -> FunctionData
getFunctionData (Func name parameters block) = (name, (parameters, Func name parameters block))

transpileFunction :: FunctionData -> String
transpileFunction funcData = "def " ++ fst funcData ++ ":\n\t" ++ "pass\n"