module Optimizer where

import AST
import Lib

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

reduceArithmeticExp :: ArithmeticExp -> ArithmeticExp
reduceArithmeticExp (Number number) = Number number
reduceArithmeticExp (NumericVar name) = NumericVar name
reduceArithmeticExp (NumericFunc func args) = NumericFunc func args
reduceArithmeticExp (Negate arith) = reduceNegation arith
reduceArithmeticExp (ArithmeticBinaryOperation op arith1 arith2) = reduceArithmeticOperation op arith1 arith2

reduceNegation :: ArithmeticExp -> ArithmeticExp
reduceNegation arith
    | isNumber (reduceArithmeticExp arith) = Number (- getNumber (reduceArithmeticExp arith))
    | otherwise = Negate (reduceArithmeticExp arith)

reduceArithmeticOperation :: ArithmeticBinaryOperator -> ArithmeticExp -> ArithmeticExp -> ArithmeticExp
reduceArithmeticOperation Add arith1 arith2 = reduceAdd arith1 arith2
reduceArithmeticOperation Minus arith1 arith2 = reduceMinus arith1 arith2
reduceArithmeticOperation Multiply arith1 arith2 = reduceMultiply arith1 arith2
reduceArithmeticOperation Divide arith1 arith2 = reduceDivide arith1 arith2
reduceArithmeticOperation Modulo arith1 arith2 = reduceModulo arith1 arith2
reduceArithmeticOperation Power arith1 arith2 = reducePower arith1 arith2

reduceAdd :: ArithmeticExp -> ArithmeticExp -> ArithmeticExp
reduceAdd arith1 arith2
    | isZero (reduceArithmeticExp arith1) = reduceArithmeticExp arith2
    | isZero (reduceArithmeticExp arith2) = reduceArithmeticExp arith1
    | isNumber (reduceArithmeticExp arith1) 
        && isNumber (reduceArithmeticExp arith2) = Number (getNumber (reduceArithmeticExp arith1) +
                                                            getNumber (reduceArithmeticExp arith2))
    | otherwise = ArithmeticBinaryOperation Add (reduceArithmeticExp arith1) (reduceArithmeticExp arith2)

reduceMinus :: ArithmeticExp -> ArithmeticExp -> ArithmeticExp
reduceMinus arith1 arith2
    | isZero (reduceArithmeticExp arith1) = reduceArithmeticExp (Negate arith2)
    | isZero (reduceArithmeticExp arith2) = reduceArithmeticExp arith1
    | isNumber (reduceArithmeticExp arith1) 
        && isNumber (reduceArithmeticExp arith2) = Number (getNumber (reduceArithmeticExp arith1) -
                                                            getNumber (reduceArithmeticExp arith2))
    | otherwise = ArithmeticBinaryOperation Minus (reduceArithmeticExp arith1) (reduceArithmeticExp arith2)

reduceMultiply :: ArithmeticExp -> ArithmeticExp -> ArithmeticExp
reduceMultiply arith1 arith2
    | isZero (reduceArithmeticExp arith1) || isZero (reduceArithmeticExp arith2) = Number 0
    | isOne (reduceArithmeticExp arith1) = reduceArithmeticExp arith2
    | isOne (reduceArithmeticExp arith2) = reduceArithmeticExp arith1
    | isNumber (reduceArithmeticExp arith1) 
        && isNumber (reduceArithmeticExp arith2) = Number (getNumber (reduceArithmeticExp arith1) *
                                                            getNumber (reduceArithmeticExp arith2))
    | otherwise = ArithmeticBinaryOperation Multiply (reduceArithmeticExp arith1) (reduceArithmeticExp arith2)

reduceDivide :: ArithmeticExp -> ArithmeticExp -> ArithmeticExp
reduceDivide arith1 arith2
    | isZero (reduceArithmeticExp arith1) = Number 0
    | isOne (reduceArithmeticExp arith2) = reduceArithmeticExp arith1
    | isNumber (reduceArithmeticExp arith1) 
        && isNumber (reduceArithmeticExp arith2) = Number (getNumber (reduceArithmeticExp arith1) `div`
                                                            getNumber (reduceArithmeticExp arith2))
    | otherwise = ArithmeticBinaryOperation Divide (reduceArithmeticExp arith1) (reduceArithmeticExp arith2)

reduceModulo :: ArithmeticExp -> ArithmeticExp -> ArithmeticExp
reduceModulo arith1 arith2
    | isZero (reduceArithmeticExp arith1) || isOne (reduceArithmeticExp arith2) = Number 0
    | isNumber (reduceArithmeticExp arith1) 
        && isNumber (reduceArithmeticExp arith2) = Number (getNumber (reduceArithmeticExp arith1) `mod`
                                                            getNumber (reduceArithmeticExp arith2))
    | otherwise = ArithmeticBinaryOperation Modulo (reduceArithmeticExp arith1) (reduceArithmeticExp arith2)

reducePower :: ArithmeticExp -> ArithmeticExp -> ArithmeticExp
reducePower arith1 arith2
    | isZero (reduceArithmeticExp arith2) || isOne (reduceArithmeticExp arith1) = Number 1
    | isZero (reduceArithmeticExp arith1) = Number 0
    | isNumber (reduceArithmeticExp arith1) 
        && isNumber (reduceArithmeticExp arith2) = Number (getNumber (reduceArithmeticExp arith1) `integerPower`
                                                            getNumber (reduceArithmeticExp arith2))
    | otherwise = ArithmeticBinaryOperation Modulo (reduceArithmeticExp arith1) (reduceArithmeticExp arith2)

reduceRelationalArithmetic :: RelationalBinaryOperator -> ArithmeticExp -> ArithmeticExp -> BoolExp
reduceRelationalArithmetic Equals arith1 arith2 = reduceArithEquals arith1 arith2
reduceRelationalArithmetic NotEquals arith1 arith2 = reduceArithNotEquals arith1 arith2
reduceRelationalArithmetic Less arith1 arith2 = reduceArithLess arith1 arith2
reduceRelationalArithmetic LessOrEqual arith1 arith2 = reduceArithLessOrEqual arith1 arith2
reduceRelationalArithmetic Greater arith1 arith2 = reduceArithGreater arith1 arith2
reduceRelationalArithmetic GreaterOrEqual arith1 arith2 = reduceArithGreaterOrEqual arith1 arith2

reduceArithEquals :: ArithmeticExp -> ArithmeticExp -> BoolExp
reduceArithEquals arith1 arith2
    | isNumber (reduceArithmeticExp arith1) 
        && isNumber (reduceArithmeticExp arith2) = if getNumber (reduceArithmeticExp arith1) == getNumber (reduceArithmeticExp arith2)
                                                    then TrueValue
                                                    else FalseValue
    | otherwise = RelationalBinaryArithmetic Equals (reduceArithmeticExp arith1) (reduceArithmeticExp arith2)

reduceArithNotEquals :: ArithmeticExp -> ArithmeticExp -> BoolExp
reduceArithNotEquals arith1 arith2 = reduceBoolExp (Not (RelationalBinaryArithmetic Equals arith1 arith2))

reduceArithLess :: ArithmeticExp -> ArithmeticExp -> BoolExp
reduceArithLess arith1 arith2
    | isNumber (reduceArithmeticExp arith1) 
        && isNumber (reduceArithmeticExp arith2) = if getNumber (reduceArithmeticExp arith1) < getNumber (reduceArithmeticExp arith2)
                                                    then TrueValue
                                                    else FalseValue
    | otherwise = RelationalBinaryArithmetic Equals (reduceArithmeticExp arith1) (reduceArithmeticExp arith2)

reduceArithLessOrEqual :: ArithmeticExp -> ArithmeticExp -> BoolExp
reduceArithLessOrEqual arith1 arith2
    | isNumber (reduceArithmeticExp arith1) 
        && isNumber (reduceArithmeticExp arith2) = if getNumber (reduceArithmeticExp arith1) <= getNumber (reduceArithmeticExp arith2)
                                                    then TrueValue
                                                    else FalseValue
    | otherwise = RelationalBinaryArithmetic Equals (reduceArithmeticExp arith1) (reduceArithmeticExp arith2)

reduceArithGreater :: ArithmeticExp -> ArithmeticExp -> BoolExp
reduceArithGreater arith1 arith2
    | isNumber (reduceArithmeticExp arith1) 
        && isNumber (reduceArithmeticExp arith2) = if getNumber (reduceArithmeticExp arith1) > getNumber (reduceArithmeticExp arith2)
                                                    then TrueValue
                                                    else FalseValue
    | otherwise = RelationalBinaryArithmetic Equals (reduceArithmeticExp arith1) (reduceArithmeticExp arith2)

reduceArithGreaterOrEqual :: ArithmeticExp -> ArithmeticExp -> BoolExp
reduceArithGreaterOrEqual arith1 arith2
    | isNumber (reduceArithmeticExp arith1) 
        && isNumber (reduceArithmeticExp arith2) = if getNumber (reduceArithmeticExp arith1) >= getNumber (reduceArithmeticExp arith2)
                                                    then TrueValue
                                                    else FalseValue
    | otherwise = RelationalBinaryArithmetic Equals (reduceArithmeticExp arith1) (reduceArithmeticExp arith2)