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
        isFalseValue,
        isTrueValue,
        isNumber,
        getNumber,
        isZero,
        isOne,
        integerPower,
        isStringConst,
        getStringVal
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

isFalseValue :: BoolExp -> Bool
isFalseValue FalseValue = True
isFalseValue _ = False

isTrueValue :: BoolExp -> Bool
isTrueValue TrueValue = True
isTrueValue _ = False

isNumber :: ArithmeticExp -> Bool
isNumber (Number _) = True
isNumber _ = False

safeGetNumber :: ArithmeticExp -> Maybe Integer
safeGetNumber (Number number) = Just number
safeGetNumber _ = Nothing

-- Only call when isNumber is True
getNumber :: ArithmeticExp -> Integer
getNumber (Number number) = number
getNumber _ = 0

isZero :: ArithmeticExp -> Bool
isZero (Number number) = number == 0
isZero _ = False

isOne :: ArithmeticExp -> Bool
isOne (Number number) = number == 1
isOne _ = False

integerPower :: Integer -> Integer -> Integer
integerPower base exponent
    | exponent == 0 || base == 1 = 1
    | base == 0 = 1
    | exponent == 1 = base
    | otherwise = base * integerPower base (exponent - 1)

isStringConst :: StringExp -> Bool
isStringConst (StringConstant _) = True
isStringConst _ = False

safeGetStringVal :: StringExp -> Maybe String
safeGetStringVal (StringConstant string) = Just string
safeGetStringVal _ = Nothing

-- Only call when isStringConst is True
getStringVal :: StringExp -> String
getStringVal (StringConstant string) = string
getStringVal _ = ""