module Optimizer 
    (
        reduceBoolExp,
        reduceArithmeticExp,
        reduceStringExp
    ) where

import AST
import Lib

reduceBoolExp :: BoolExp -> BoolExp
reduceBoolExp (BoolBinaryOperations op bool1 bool2) = reduceBoolOperation op bool1 bool2
reduceBoolExp (Not bool) = reduceNot bool
reduceBoolExp (RelationalBinaryArithmetic op arith1 arith2) = reduceRelationalArithmetic op arith1 arith2
reduceBoolExp (RelationalBinaryString op string1 string2) = reduceRelationalString op string1 string2
reduceBoolExp boolExp = boolExp

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

reduceArithmeticExp :: ArithmeticExp -> ArithmeticExp
reduceArithmeticExp (Negate arith) = reduceNegation arith
reduceArithmeticExp (ArithmeticBinaryOperation op arith1 arith2) = reduceArithmeticOperation op arith1 arith2
reduceArithmeticExp arithExp = arithExp

reduceNegation :: ArithmeticExp -> ArithmeticExp
reduceNegation arith
    | isNumber (reduceArithmeticExp arith) = Number (- getNumber (reduceArithmeticExp arith))
    | otherwise = Negate (reduceArithmeticExp arith)

reduceArithmeticOperation :: ArithmeticBinaryOperator -> ArithmeticExp -> ArithmeticExp -> ArithmeticExp
reduceArithmeticOperation Add arith1 arith2 = reduceAdd (reduceArithmeticExp arith1) (reduceArithmeticExp arith2)
reduceArithmeticOperation Minus arith1 arith2 = reduceMinus (reduceArithmeticExp arith1) (reduceArithmeticExp arith2)
reduceArithmeticOperation Multiply arith1 arith2 = reduceMultiply (reduceArithmeticExp arith1) (reduceArithmeticExp arith2)
reduceArithmeticOperation Divide arith1 arith2 = reduceDivide (reduceArithmeticExp arith1) (reduceArithmeticExp arith2)
reduceArithmeticOperation Modulo arith1 arith2 = reduceModulo (reduceArithmeticExp arith1) (reduceArithmeticExp arith2)
reduceArithmeticOperation Power arith1 arith2 = reducePower (reduceArithmeticExp arith1) (reduceArithmeticExp arith2)

reduceAdd :: ArithmeticExp -> ArithmeticExp -> ArithmeticExp
reduceAdd reduced1 reduced2
    | isZero reduced1 = reduceArithmeticExp reduced2
    | isZero (reduceArithmeticExp reduced2) = reduced1
    | otherwise = reduceGenericArithOperation reduced1 reduced2 (+) Add
    
reduceMinus :: ArithmeticExp -> ArithmeticExp -> ArithmeticExp
reduceMinus reduced1 reduced2
    | isZero reduced1 = reduceArithmeticExp (Negate reduced2)
    | isZero reduced2 = reduced1
    | otherwise = reduceGenericArithOperation reduced1 reduced2 (-) Minus
    
reduceMultiply :: ArithmeticExp -> ArithmeticExp -> ArithmeticExp
reduceMultiply reduced1 reduced2
    | isZero reduced1 || isZero reduced2 = Number 0
    | isOne reduced1 = reduced2
    | isOne reduced2 = reduced1
    | otherwise = reduceGenericArithOperation reduced1 reduced2 (*) Multiply
    
reduceDivide :: ArithmeticExp -> ArithmeticExp -> ArithmeticExp
reduceDivide reduced1 reduced2
    | isZero reduced1 = Number 0
    | isOne reduced2 = reduced1
    | otherwise = reduceGenericArithOperation reduced1 reduced2 div Divide
    
reduceModulo :: ArithmeticExp -> ArithmeticExp -> ArithmeticExp
reduceModulo reduced1 reduced2
    | isZero reduced1 || isOne reduced2 = Number 0
    | otherwise = reduceGenericArithOperation reduced1 reduced2 mod Modulo

reducePower :: ArithmeticExp -> ArithmeticExp -> ArithmeticExp
reducePower reduced1 reduced2
    | isZero reduced2 || isOne reduced1 = Number 1
    | isZero reduced1 = Number 0
    | otherwise = reduceGenericArithOperation reduced1 reduced2 integerPower Power
    
reduceGenericArithOperation :: ArithmeticExp -> ArithmeticExp -> (Integer -> Integer -> Integer) -> ArithmeticBinaryOperator -> ArithmeticExp
reduceGenericArithOperation reduced1 reduced2 f op
    | isNumber reduced1 && isNumber reduced2 = Number (f (getNumber reduced1) (getNumber reduced2))
    | otherwise = ArithmeticBinaryOperation op reduced1 reduced2

reduceRelationalArithmetic :: RelationalBinaryOperator -> ArithmeticExp -> ArithmeticExp -> BoolExp
reduceRelationalArithmetic Equals arith1 arith2 = reduceRelationalArithOp (reduceArithmeticExp arith1) 
                                                                            (reduceArithmeticExp arith2)
                                                                            (==) Equals
reduceRelationalArithmetic NotEquals arith1 arith2 = reduceRelationalArithOp (reduceArithmeticExp arith1) 
                                                                            (reduceArithmeticExp arith2)
                                                                            (/=) NotEquals
reduceRelationalArithmetic Less arith1 arith2 = reduceRelationalArithOp (reduceArithmeticExp arith1) 
                                                                        (reduceArithmeticExp arith2)
                                                                        (<) Less
reduceRelationalArithmetic LessOrEqual arith1 arith2 = reduceRelationalArithOp (reduceArithmeticExp arith1) 
                                                                                (reduceArithmeticExp arith2)
                                                                                (<=) LessOrEqual
reduceRelationalArithmetic Greater arith1 arith2 = reduceRelationalArithOp (reduceArithmeticExp arith1) 
                                                                            (reduceArithmeticExp arith2)
                                                                            (>) Greater 
reduceRelationalArithmetic GreaterOrEqual arith1 arith2 = reduceRelationalArithOp (reduceArithmeticExp arith1) 
                                                                                    (reduceArithmeticExp arith2)
                                                                                    (>=) GreaterOrEqual

reduceRelationalArithOp :: ArithmeticExp -> ArithmeticExp -> (Integer -> Integer -> Bool) -> RelationalBinaryOperator -> BoolExp
reduceRelationalArithOp arith1 arith2 f op
    | isNumber arith1 && isNumber arith2 = if f (getNumber arith1) (getNumber arith2) then TrueValue else FalseValue
    | otherwise = RelationalBinaryArithmetic op arith1 arith2

reduceStringExp :: StringExp  -> StringExp
reduceStringExp (StringBinaryOperation op string1 string2) = reduceStringOperation op string1 string2
reduceStringExp stringExp = stringExp

reduceStringOperation :: StringOperators -> StringExp  -> StringExp -> StringExp
reduceStringOperation Concat string1 string2 = reduceConcat (reduceStringExp string1) (reduceStringExp string2)

reduceConcat :: StringExp -> StringExp -> StringExp
reduceConcat reducedString1 reducedString2
    | isStringConst reducedString1 && isStringConst reducedString2 = StringConstant (getStringVal reducedString1 ++ 
                                                                                        getStringVal reducedString2)
    | otherwise = StringBinaryOperation Concat reducedString1 reducedString2

reduceRelationalString :: RelationalBinaryOperator -> StringExp  -> StringExp  -> BoolExp
reduceRelationalString Equals string1 string2 = reduceRelationalStringOp (reduceStringExp string1) 
                                                                            (reduceStringExp string2)
                                                                            (==) Equals
reduceRelationalString NotEquals string1 string2 = reduceRelationalStringOp (reduceStringExp string1) 
                                                                            (reduceStringExp string2)
                                                                            (/=) NotEquals
reduceRelationalString Less string1 string2 = reduceRelationalStringOp (reduceStringExp string1) 
                                                                        (reduceStringExp string2)
                                                                        (<) Less
reduceRelationalString LessOrEqual string1 string2 = reduceRelationalStringOp (reduceStringExp string1) 
                                                                                (reduceStringExp string2)
                                                                                (<=) LessOrEqual
reduceRelationalString Greater string1 string2 = reduceRelationalStringOp (reduceStringExp string1) 
                                                                            (reduceStringExp string2)
                                                                            (>) Greater 
reduceRelationalString GreaterOrEqual string1 string2 = reduceRelationalStringOp (reduceStringExp string1) 
                                                                                    (reduceStringExp string2)
                                                                                    (>=) GreaterOrEqual

reduceRelationalStringOp :: StringExp  -> StringExp  -> (String -> String -> Bool) -> RelationalBinaryOperator -> BoolExp
reduceRelationalStringOp string1 string2 f op
    | isStringConst string1 && isStringConst string2 = if f (getStringVal string1) (getStringVal string2) 
                                                        then TrueValue else FalseValue
    | otherwise = RelationalBinaryString op string1 string2
