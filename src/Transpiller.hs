module Transpiller where

import AST

-- Probabbly make a function that returns [(FuncName, ([Parameters, FuncAST))] from Program
-- hasMain $ map fst pairTest where pairTest is the array of [(FuncName, ([Parameters, FuncAST))]
-- parameters to check if quantity matches on every call, funcName to check for main and to check function exists

hasMain :: [String] -> Bool
hasMain funcNames = (== 1) . length $ filter (== "main") funcNames

-- getFunction :: [(String, (Int, Function))] -> Name -> Maybe (String, (Int, Function))
getFunction :: [(String, (Int, String))] -> String -> Maybe (String, (Int, String))
getFunction functions functionName
    | length functionInfo == 1 = Just (head functionInfo)
    | otherwise = Nothing -- Not declared or redeclared
    where functionInfo = filter ((== functionName) . fst) functions

-- isValidFuncCall :: (String, (Int, Function)) -> [(String, (Int, Function))] -> Bool
isValidFuncCall :: (String, (Int, String)) -> [(String, (Int, String))] -> Bool
isValidFuncCall call functions = case function of
                            Nothing -> False
                            Just f -> (fst . snd) call == (fst . snd) f 
                        where function = getFunction functions (fst call)