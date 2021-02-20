module OptimizerTest where

import Test.HUnit

import AST
import Optimizer
import Lib

-- data for tests
falseAnd :: BoolExp
falseAnd = BoolBinaryOperations And (BoolBinaryOperations And (BoolBinaryOperations And TrueValue TrueValue) TrueValue)
                                     (BoolBinaryOperations And TrueValue FalseValue)

trueAnd :: BoolExp
trueAnd = BoolBinaryOperations And (BoolBinaryOperations And (BoolBinaryOperations And TrueValue TrueValue) TrueValue)
                                     (BoolBinaryOperations And TrueValue TrueValue)

dontReduceAnd :: BoolExp
dontReduceAnd = BoolBinaryOperations And (BoolBinaryOperations And (BoolBinaryOperations And (BoolVar "a") TrueValue) TrueValue)
                                     (BoolBinaryOperations And TrueValue TrueValue)

falseOr :: BoolExp
falseOr = BoolBinaryOperations Or (BoolBinaryOperations Or (BoolBinaryOperations Or FalseValue FalseValue) FalseValue)
                                     (BoolBinaryOperations Or FalseValue (Not TrueValue))

trueOr :: BoolExp
trueOr = BoolBinaryOperations Or (BoolBinaryOperations Or (BoolBinaryOperations Or FalseValue FalseValue) FalseValue)
                                     (BoolBinaryOperations Or TrueValue FalseValue)

dontReduceOr :: BoolExp
dontReduceOr = BoolBinaryOperations Or (BoolBinaryOperations Or (BoolBinaryOperations Or FalseValue FalseValue) FalseValue)
                                     (BoolBinaryOperations Or (BoolFunc "func" []) FalseValue)
mixedTrue :: BoolExp
mixedTrue = BoolBinaryOperations Or (BoolBinaryOperations And (BoolBinaryOperations Or TrueValue FalseValue) (Not FalseValue))
                                     (BoolBinaryOperations Or FalseValue FalseValue)
mixedFalse :: BoolExp
mixedFalse = BoolBinaryOperations And (BoolBinaryOperations And (BoolBinaryOperations Or TrueValue (Not FalseValue)) FalseValue)
                                     (BoolBinaryOperations Or (Not FalseValue) FalseValue)

reducibleSum :: ArithmeticExp
reducibleSum = ArithmeticBinaryOperation Add (Number 4) (ArithmeticBinaryOperation Add (ArithmeticBinaryOperation Add (Number 5) (Number 5)) (Number 6))

reducibleSumWithNeg :: ArithmeticExp
reducibleSumWithNeg = ArithmeticBinaryOperation Add (Negate (Number 4)) (ArithmeticBinaryOperation Add (ArithmeticBinaryOperation Add (Number 5) (Number 5)) (Number 6))

reducibleMinus :: ArithmeticExp
reducibleMinus = ArithmeticBinaryOperation Minus (Number 4) (ArithmeticBinaryOperation Minus (ArithmeticBinaryOperation Minus (Number 5) (Number 5)) (Number 6))

reducibleMultiply :: ArithmeticExp
reducibleMultiply = ArithmeticBinaryOperation Multiply (Number 2) (ArithmeticBinaryOperation Multiply (ArithmeticBinaryOperation Minus (Number 3) (Number 5)) (Number 4))

reducibleDivide :: ArithmeticExp
reducibleDivide = ArithmeticBinaryOperation Divide (Number 4) (ArithmeticBinaryOperation Divide (ArithmeticBinaryOperation Divide (Number 8) (Number 2)) (Number 2))

reducibleModulo :: ArithmeticExp
reducibleModulo = ArithmeticBinaryOperation Modulo (Number 4) (ArithmeticBinaryOperation Modulo (ArithmeticBinaryOperation Modulo (Number 9) (Number 10)) (Number 17))

reduciblePower :: ArithmeticExp
reduciblePower = ArithmeticBinaryOperation Power (Number 2) (ArithmeticBinaryOperation Power (ArithmeticBinaryOperation Power (Number 2) (Number 3)) (Number 1))

reducibleArithMixed :: ArithmeticExp
reducibleArithMixed = ArithmeticBinaryOperation Multiply (Number 2) (ArithmeticBinaryOperation Power (ArithmeticBinaryOperation Add (Number 2) (Number 3)) (Number 2))

reducibleStringOperation :: StringExp
reducibleStringOperation = StringBinaryOperation Concat (StringBinaryOperation Concat (StringConstant "Hello ") (StringConstant "World")) (StringConstant "!")

-- tests
reduceAnd1 :: Test
reduceAnd1 = TestCase (assertEqual "for reduceBoolExp with false and" FalseValue (reduceBoolExp falseAnd))

reduceAnd2 :: Test
reduceAnd2 = TestCase (assertEqual "for reduceBoolExp with true and" TrueValue (reduceBoolExp trueAnd))

reduceAnd3 :: Test
reduceAnd3 = TestCase (assertBool "for reduceBoolExp with irreducible and" (FalseValue /= reduceBoolExp dontReduceAnd
                                                                            && TrueValue /= reduceBoolExp dontReduceAnd))

reduceOr1 :: Test
reduceOr1 = TestCase (assertEqual "for reduceBoolExp with false or" FalseValue (reduceBoolExp falseOr))

reduceOr2 :: Test
reduceOr2 = TestCase (assertEqual "for reduceBoolExp with true or" TrueValue (reduceBoolExp trueOr))

reduceOr3 :: Test
reduceOr3 = TestCase (assertBool "for reduceBoolExp with irreducible or" (FalseValue /= reduceBoolExp dontReduceOr
                                                                            && TrueValue /= reduceBoolExp dontReduceOr))

reduceMixed1 :: Test
reduceMixed1 = TestCase (assertEqual "for reduceBoolExp with false mixed" FalseValue (reduceBoolExp mixedFalse))

reduceMixed2 :: Test
reduceMixed2 = TestCase (assertEqual "for reduceBoolExp with true mixed" TrueValue (reduceBoolExp mixedTrue))

reduceSum1 :: Test
reduceSum1 = TestCase (assertEqual "for reduceArithmeticExp with simple add" (Number 20) (reduceArithmeticExp reducibleSum))

reduceSum2 :: Test
reduceSum2 = TestCase (assertEqual "for reduceArithmeticExp with add and neg" (Number 12) (reduceArithmeticExp reducibleSumWithNeg))

reduceMinus1 :: Test
reduceMinus1 = TestCase (assertEqual "for reduceArithmeticExp simple minus" (Number 10) (reduceArithmeticExp reducibleMinus))

reduceMultiply1 :: Test
reduceMultiply1 = TestCase (assertEqual "for reduceArithmeticExp simple multiply" (Number (-16)) (reduceArithmeticExp reducibleMultiply))

reduceDivide1 :: Test
reduceDivide1 = TestCase (assertEqual "for reduceArithmeticExp simple division" (Number 2) (reduceArithmeticExp reducibleDivide))

reduceModulo1 :: Test
reduceModulo1 = TestCase (assertEqual "for reduceArithmeticExp simple modulo" (Number 4) (reduceArithmeticExp reducibleModulo))

reducePower1 :: Test
reducePower1 = TestCase (assertEqual "for reduceArithmeticExp simple modulo" (Number 256) (reduceArithmeticExp reduciblePower))

reduceArithMixed :: Test
reduceArithMixed = TestCase (assertEqual "for reduceArithmeticExp mixed" (Number 50) (reduceArithmeticExp reducibleArithMixed))

reduceLessArith :: Test
reduceLessArith = TestCase (assertEqual "for reduceRelationalArithmetic less" TrueValue (reduceRelationalArithmetic Less (Number 100) (Number 1000)))

reduceGreaterArith :: Test
reduceGreaterArith = TestCase (assertEqual "for reduceRelationalArithmetic greater" FalseValue (reduceRelationalArithmetic Greater (Number 100) (Number 1000)))

reduceRelationalMixedArith :: Test
reduceRelationalMixedArith = TestCase (assertEqual "for reduceRelationalArithmetic mixed" FalseValue (reduceRelationalArithmetic Greater reducibleArithMixed reduciblePower))

reduceStringOperation :: Test
reduceStringOperation = TestCase (assertEqual "for reduceStringOperation concat" "Hello World!" (getStringVal (reduceStringExp reducibleStringOperation)))

reduceLessString :: Test
reduceLessString = TestCase (assertEqual "for reduceRelationalString less" FalseValue (reduceRelationalString Less (StringConstant "hola") (StringConstant "hello")))

reduceGreaterString :: Test
reduceGreaterString = TestCase (assertEqual "for reduceRelationalString greater" FalseValue (reduceRelationalString Less (StringConstant "hola") (StringConstant "hello")))

getOptimizerTests :: Test
getOptimizerTests = TestList [TestLabel "reduceBoolExp with false and" reduceAnd1,
                  TestLabel "reduceBoolExp with true and" reduceAnd2,
                  TestLabel "reduceBoolExp with irreducible and" reduceAnd3,
                  TestLabel "reduceBoolExp with false or" reduceOr1,
                  TestLabel "reduceBoolExp with true or" reduceOr2,
                  TestLabel "reduceBoolExp with irreducible or" reduceOr3,
                  TestLabel "reduceBoolExp with false mixed" reduceMixed1,
                  TestLabel "reduceBoolExp with true mixed" reduceMixed2,
                  TestLabel "reduceArithmeticlExp with simple add" reduceSum1,
                  TestLabel "reduceArithmeticlExp with add and neg" reduceSum2,
                  TestLabel "reduceArithmeticlExp with simple minus" reduceMinus1,
                  TestLabel "reduceArithmeticlExp with simple multiply" reduceMultiply1,
                  TestLabel "reduceArithmeticlExp with simple division" reduceDivide1,
                  TestLabel "reduceArithmeticlExp with simple modulo" reduceModulo1,
                  TestLabel "reduceArithmeticlExp with simple power" reducePower1,
                  TestLabel "reduceArithmeticlExp mixed" reduceArithMixed,
                  TestLabel "reduceRelationalArithmeticlExp less" reduceLessArith,
                  TestLabel "reduceRelationalArithmeticlExp greater" reduceGreaterArith,
                  TestLabel "reduceRelationalArithmeticlExp mixed" reduceRelationalMixedArith,
                  TestLabel "reduceStringOperation concat" reduceStringOperation,
                  TestLabel "reduceRelationalStringExp less" reduceLessString,
                  TestLabel "reduceRelationalStringExp greater" reduceGreaterString]
                