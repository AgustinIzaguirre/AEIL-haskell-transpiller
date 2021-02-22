module Transpiller where

import Data.List

import AST
import Lib
import ErrorMessages
import Optimizer

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
transpileBlock block level 
    | null blockStatementsResults = Right (identForLevel level ++ "pass\n")
    | hasProgramError blockStatementsResults = Left (getProgramErrors blockStatementsResults)
    | otherwise = Right (concatMap unwrap blockStatementsResults)
    where blockStatementsResults = fmap (flip transpileStatement level) (getBlockStatements block)

transpileStatement :: Statement -> Int -> Either String String
transpileStatement (Assign name value) level = errorOrValue (transpileAssignStatement name (reduceValueExp value) level)
transpileStatement (Return value) level = errorOrValue (transpileReturnStatement (reduceValueExp value) level)
transpileStatement (If condition block) level = errorOrValue (transpileIfStatement (reduceBoolExp condition) block level)
transpileStatement (IfElse condition ifBlock elseBlock) level = errorOrValue (transpileIfElseStatement (reduceBoolExp condition) 
                                                                                ifBlock elseBlock level)
transpileStatement (While condition block) level = errorOrValue (transpileWhileStatement (reduceBoolExp condition) block level)
transpileStatement (PrintFunc text) level = errorOrValue (transpilePrintStatement (reduceValueExp text) level)
transpileStatement (FuncCall name args) level = errorOrValue (transpileFuncCallStatement name (fmap reduceValueExp args) level)

transpileAssignStatement :: String -> ValueExp -> Int -> Either String String
transpileAssignStatement name value level = errorOr (transpileValueExp value) (identForLevel level ++ name ++ " = ") "\n"

transpileReturnStatement :: ValueExp -> Int -> Either String String
transpileReturnStatement value level = errorOr (transpileValueExp value) (identForLevel level ++ "return ") "\n"

transpileIfStatement :: BoolExp  -> Block -> Int -> Either String String
transpileIfStatement condition block level 
    | hasError conditionResult = errorOrValue conditionResult
    | isTrueValue condition = errorOrValue (transpileBlock block level)
    | isFalseValue condition = Right ""
    | otherwise = errorOrPrepend (transpileBlock block (level + 1)) 
                                    (identForLevel level ++ "if " ++ unwrap conditionResult ++ ":\n")
    where conditionResult = transpileBoolExp condition

transpileIfElseStatement :: BoolExp  -> Block -> Block -> Int -> Either String String
transpileIfElseStatement condition ifBlock elseBlock level 
    | hasError ifResult = errorOrValue ifResult
    | isTrueValue condition = errorOrValue (transpileBlock ifBlock level)
    | isFalseValue condition = errorOrValue (transpileBlock elseBlock level)
    | otherwise = errorOrPrepend (transpileBlock elseBlock (level + 1)) (unwrap ifResult ++ identForLevel level ++ "else:\n")
    where ifResult = transpileIfStatement condition ifBlock level

transpileWhileStatement :: BoolExp  -> Block -> Int -> Either String String
transpileWhileStatement condition block level 
    | hasError conditionResult = errorOrValue conditionResult
    | isFalseValue condition = Right ""
    | otherwise = errorOrPrepend (transpileBlock block (level + 1)) 
                                    (identForLevel level ++ "while " ++ unwrap conditionResult ++ ":\n")
    where conditionResult = transpileBoolExp condition

transpilePrintStatement :: ValueExp -> Int -> Either String String
transpilePrintStatement text level = errorOr (transpileValueExp text) (identForLevel level ++ "print(") ", end=\"\")\n"

transpileFuncCallStatement :: String -> [ValueExp] -> Int -> Either String String 
transpileFuncCallStatement name args level = errorOr (transpileFuncCallValue name args) (identForLevel level) "\n"

-- Received bool expression should be reduced before calling
transpileBoolExp :: BoolExp -> Either String String 
transpileBoolExp TrueValue = Right "True"
transpileBoolExp FalseValue = Right "False"
transpileBoolExp (BoolVar name) = Right name
transpileBoolExp (BoolFunc func args) = errorOrValue (transpileFuncCallValue func args)
transpileBoolExp (BoolBinaryOperations op bool1 bool2) = errorOrValue (transpileBoolOperation op bool1 bool2)
transpileBoolExp (Not bool) = errorOrValue (transpileNotBoolExp bool)
transpileBoolExp (RelationalBinaryArithmetic op arith1 arith2) = errorOrValue (transpileRelationalArithmetic op arith1 arith2)
transpileBoolExp (RelationalBinaryString op string1 string2) = errorOrValue (transpileRelationalString op string1 string2)

-- assuming operation is irreducible
transpileBoolOperation :: BoolBinaryOperators -> BoolExp -> BoolExp -> Either String String
transpileBoolOperation op bool1 bool2
    | hasError bool1Result = errorOrValue bool1Result
    | hasError (transpileBoolBinaryOperators op) = errorOrValue (transpileBoolBinaryOperators op)
    | otherwise = errorOr (transpileBoolExp bool2) (" ( " ++ unwrap bool1Result ++ unwrap (transpileBoolBinaryOperators op)) " )"
    where bool1Result = transpileBoolExp bool1

transpileBoolBinaryOperators :: BoolBinaryOperators  -> Either String String
transpileBoolBinaryOperators And = Right " and "
transpileBoolBinaryOperators Or = Right " or "

transpileNotBoolExp :: BoolExp -> Either String String
transpileNotBoolExp bool = errorOr (transpileBoolExp bool) "not ( " " )"

transpileRelationalArithmetic :: RelationalBinaryOperator -> ArithmeticExp -> ArithmeticExp -> Either String String
transpileRelationalArithmetic op arith1 arith2
    | hasError arith1Result = errorOrValue arith1Result
    | hasError (transpileRelationalOperator op) = errorOrValue (transpileRelationalOperator op)
    | otherwise = errorOr (transpileArithmeticExp arith2) (" ( " ++ unwrap arith1Result++ unwrap (transpileRelationalOperator op)) " )"
    where arith1Result = transpileArithmeticExp arith1

transpileRelationalString :: RelationalBinaryOperator -> StringExp  -> StringExp -> Either String String
transpileRelationalString op string1 string2
    | hasError string1Result = errorOrValue string1Result
    | hasError (transpileRelationalOperator op) = errorOrValue (transpileRelationalOperator op)
    | otherwise = errorOr (transpileStringExp string2) (" ( " ++ unwrap string1Result ++ unwrap (transpileRelationalOperator op)) " )"
    where string1Result = transpileStringExp string1

transpileRelationalOperator :: RelationalBinaryOperator -> Either String String
transpileRelationalOperator Equals = Right " == "
transpileRelationalOperator NotEquals = Right " != "
transpileRelationalOperator Less = Right " < "
transpileRelationalOperator LessOrEqual = Right " <= "
transpileRelationalOperator Greater = Right " > "
transpileRelationalOperator GreaterOrEqual = Right " >= "

-- expression is irreducible and is called reduceArithmetic before transpileArithmeticExp
transpileArithmeticExp :: ArithmeticExp -> Either String String 
transpileArithmeticExp (Number number) = Right (show number)
transpileArithmeticExp (NumericVar name) = Right name
transpileArithmeticExp (NumericFunc func args) = errorOrValue (transpileFuncCallValue func args)
transpileArithmeticExp (Negate arith) = errorOrPrepend (transpileArithmeticExp arith) "-"
transpileArithmeticExp (ArithmeticBinaryOperation op arith1 arith2) = errorOrValue (transpileArithmeticOperation op arith1 arith2)

-- Asuming operation is irreducible
transpileArithmeticOperation :: ArithmeticBinaryOperator -> ArithmeticExp  -> ArithmeticExp  -> Either String String
transpileArithmeticOperation op arith1 arith2
    | hasError arith1Result = errorOrValue arith1Result
    | hasError (transpileArithmeticOperators op) = errorOrValue (transpileArithmeticOperators op)
    | otherwise = errorOr (transpileArithmeticExp arith2) 
                                    ("(" ++ unwrap arith1Result ++ unwrap (transpileArithmeticOperators op))
                                    ")"
    where arith1Result = transpileArithmeticExp arith1

transpileArithmeticOperators :: ArithmeticBinaryOperator  -> Either String String
transpileArithmeticOperators Add = Right " + "
transpileArithmeticOperators Minus = Right " - "
transpileArithmeticOperators Multiply = Right " * "
transpileArithmeticOperators Divide = Right " // "
transpileArithmeticOperators Modulo = Right " % "
transpileArithmeticOperators Power = Right " ** "

transpileStringExp :: StringExp -> Either String String 
transpileStringExp (StringConstant text) = Right ("\"" ++ text ++ "\"")
transpileStringExp (StringVar var) = Right var
transpileStringExp (StringFunc func args) = errorOrValue (transpileFuncCallValue func args)
transpileStringExp (StringBinaryOperation operator string1 string2) = errorOrValue (transpileStringOperation operator string1 string2)

transpileFuncCallValue :: String -> [ValueExp] -> Either String String
transpileFuncCallValue name args = errorOr (transpileArguments args) (name ++ "(") ")"

transpileArguments :: [ValueExp] -> Either String String
transpileArguments args 
    | hasProgramError argsResult = Left (getProgramErrors argsResult)
    | otherwise = Right (intercalate ", " (fmap unwrap argsResult))
    where argsResult = fmap transpileValueExp args

transpileStringOperation :: StringOperators -> StringExp  -> StringExp -> Either String String
transpileStringOperation Concat string1 string2 = errorOrValue (transpileStringConcatenation string1 string2)

transpileStringConcatenation :: StringExp -> StringExp -> Either String String
transpileStringConcatenation string1 string2
    | hasError string2Result = errorOrValue string2Result
    | otherwise = errorOrAppend(transpileStringExp string1) (" + " ++ unwrap string2Result) 
    where string2Result = transpileStringExp string2

transpileValueExp:: ValueExp -> Either String String
transpileValueExp (BoolValue boolExp) = errorOrValue (transpileBoolExp boolExp)
transpileValueExp (NumberValue arithmeticExp) = errorOrValue (transpileArithmeticExp arithmeticExp )
transpileValueExp (StringValue stringExp) = errorOrValue (transpileStringExp stringExp)
transpileValueExp (Apply func args) = errorOrValue (transpileFuncCallValue func args)
transpileValueExp (Var name) = Right name
transpileValueExp (Read text) = errorOrValue (transpileReadFunc text)
transpileValueExp (GetNumber text) = errorOrValue (transpileGetNumberFunc text)

transpileReadFunc :: StringExp -> Either String String
transpileReadFunc stringExp = errorOr (transpileStringExp stringExp) "input(" ")"

transpileGetNumberFunc :: StringExp -> Either String String
transpileGetNumberFunc stringExp = errorOr (transpileStringExp stringExp) "int(input(" "))"
