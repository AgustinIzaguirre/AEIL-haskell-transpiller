module Lib
    ( 
        FunctionData,
        identForLevel,
        hasMainFunction,
        getFunction,
        isValidFuncCall,
        getProgramFunctions,
        getFunctionData,
        getFunctionBlock,
        hasProgramError,
        getProgramErrors,
        hasError,
        unwrap,
        errorOrValue,
        errorOr,
        errorOrPrepend,
        errorOrAppend,
        getBlockStatements,
        reduceBoolExp
    ) where

import Data.Foldable
import Data.List
import AST
type FunctionData = (String,([String], Function))


identForLevel :: Int -> String
identForLevel level = iterate (++"\t") "" !! level

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

getProgramFunctions :: Program -> [FunctionData]
getProgramFunctions (Root function) = [getFunctionData function]
getProgramFunctions (Multiple function program) = getFunctionData function : getProgramFunctions program

getFunctionData :: Function -> FunctionData
getFunctionData (Func name parameters block) = (name, (parameters, Func name parameters block))

getFunctionBlock :: Function -> Block
getFunctionBlock (Func _ _ block) = block

hasError :: Either String String -> Bool
hasError (Left _) = True
hasError _ = False

hasProgramError :: [Either String String] -> Bool
hasProgramError results = or (fmap hasError results)

getProgramErrors :: [Either String String] -> String 
getProgramErrors results
    | hasProgramError results = intercalate "  " $ fmap (either id id) (filter hasError results)
    | otherwise = ""

unwrap :: Either String String -> String
unwrap = either id id

getBlockStatements :: Block -> [Statement]
getBlockStatements Empty = []
getBlockStatements (SingleAction statement) = [statement]
getBlockStatements (Actions statement block) = statement : getBlockStatements block

errorOr :: Either String String -> String -> String -> Either String String
errorOr (Left errorMessage) prev post = Left errorMessage
errorOr (Right code) prev post = Right (prev ++ code ++ post)

errorOrPrepend :: Either String String -> String -> Either String String
errorOrPrepend result prev = errorOr result prev ""

errorOrAppend :: Either String String -> String -> Either String String
errorOrAppend result post = errorOr result "" post

errorOrValue :: Either String String -> Either String String
errorOrValue result = errorOr result "" ""

-- TODO also reduce relational expressions
reduceBoolExp :: BoolExp -> BoolExp
reduceBoolExp TrueValue = TrueValue
reduceBoolExp FalseValue = FalseValue
reduceBoolExp (BoolVar name) = BoolVar name
reduceBoolExp (BoolFunc func args) = BoolFunc func args
reduceBoolExp (BoolBinaryOperations op bool1 bool2) = reduceBoolOperation op bool1 bool2
reduceBoolExp (Not bool) = reduceNot bool
reduceBoolExp (RelationalBinaryArithmetic op arith1 arith2) = RelationalBinaryArithmetic op arith1 arith2
reduceBoolExp (RelationalBinaryString op string1 string2) = RelationalBinaryString op string1 string2

reduceBoolOperation :: BoolBinaryOperators -> BoolExp -> BoolExp -> BoolExp
reduceBoolOperation And bool1 bool2 = reduceAnd bool1 bool2
reduceBoolOperation Or bool1 bool2 = reduceOr bool1 bool2

reduceAnd :: BoolExp -> BoolExp -> BoolExp
reduceAnd bool1 bool2
    | isFalseValue (reduceBoolExp bool1) || isFalseValue (reduceBoolExp bool2) = FalseValue
    | isTrueValue (reduceBoolExp bool1) = reduceBoolExp bool2
    | isTrueValue (reduceBoolExp bool2) = reduceBoolExp bool1
    | otherwise = BoolBinaryOperations And (reduceBoolExp bool1) (reduceBoolExp bool2)

reduceOr :: BoolExp -> BoolExp -> BoolExp
reduceOr bool1 bool2
    | isTrueValue (reduceBoolExp bool1) || isTrueValue (reduceBoolExp bool2) = TrueValue
    | isFalseValue (reduceBoolExp bool1) = reduceBoolExp bool2
    | otherwise = BoolBinaryOperations Or (reduceBoolExp bool1) (reduceBoolExp bool2)

reduceNot :: BoolExp -> BoolExp
reduceNot bool
    | isTrueValue (reduceBoolExp bool) = FalseValue
    | isFalseValue (reduceBoolExp bool) = TrueValue
    | otherwise = Not (reduceBoolExp bool)

isFalseValue :: BoolExp -> Bool
isFalseValue FalseValue = True
isFalseValue _ = False

isTrueValue :: BoolExp -> Bool
isTrueValue TrueValue = True
isTrueValue _ = False