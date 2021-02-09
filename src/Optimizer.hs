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
