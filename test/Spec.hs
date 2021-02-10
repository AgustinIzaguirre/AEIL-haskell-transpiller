import Test.HUnit
import Lib
import AST
import Optimizer
import OptimizerTest

emptyFunction :: String -> [String] -> Function
emptyFunction name params = Func name params Empty

-- data
functionsWithoutMain :: [FunctionData]
functionsWithoutMain = [("func1",([], emptyFunction "func1" [])), ("func2",([], emptyFunction "func2" []))]

functionsWithMainWithParams :: [FunctionData]
functionsWithMainWithParams = [("func1",([], emptyFunction "func1" [])), ("func2",([], emptyFunction "func2" [])), ("main",(["param1"], emptyFunction "main" ["param1"]))]

functionsWithValidMain :: [FunctionData]
functionsWithValidMain = [("func1",([], emptyFunction "func1" [])), ("func2",([], emptyFunction "func2" [])), ("main",([], emptyFunction "main" []))]

-- falseAnd :: BoolExp
-- falseAnd = BoolBinaryOperations And (BoolBinaryOperations And (BoolBinaryOperations And TrueValue TrueValue) TrueValue)
--                                      (BoolBinaryOperations And TrueValue FalseValue)

-- trueAnd :: BoolExp
-- trueAnd = BoolBinaryOperations And (BoolBinaryOperations And (BoolBinaryOperations And TrueValue TrueValue) TrueValue)
--                                      (BoolBinaryOperations And TrueValue TrueValue)

-- dontReduceAnd :: BoolExp
-- dontReduceAnd = BoolBinaryOperations And (BoolBinaryOperations And (BoolBinaryOperations And (BoolVar "a") TrueValue) TrueValue)
--                                      (BoolBinaryOperations And TrueValue TrueValue)

-- falseOr :: BoolExp
-- falseOr = BoolBinaryOperations Or (BoolBinaryOperations Or (BoolBinaryOperations Or FalseValue FalseValue) FalseValue)
--                                      (BoolBinaryOperations Or FalseValue (Not TrueValue))

-- trueOr :: BoolExp
-- trueOr = BoolBinaryOperations Or (BoolBinaryOperations Or (BoolBinaryOperations Or FalseValue FalseValue) FalseValue)
--                                      (BoolBinaryOperations Or TrueValue FalseValue)

-- dontReduceOr :: BoolExp
-- dontReduceOr = BoolBinaryOperations Or (BoolBinaryOperations Or (BoolBinaryOperations Or FalseValue FalseValue) FalseValue)
--                                      (BoolBinaryOperations Or (BoolFunc "func" []) FalseValue)
-- mixedTrue :: BoolExp
-- mixedTrue = BoolBinaryOperations Or (BoolBinaryOperations And (BoolBinaryOperations Or TrueValue FalseValue) (Not FalseValue))
--                                      (BoolBinaryOperations Or FalseValue FalseValue)
-- mixedFalse :: BoolExp
-- mixedFalse = BoolBinaryOperations And (BoolBinaryOperations And (BoolBinaryOperations Or TrueValue (Not FalseValue)) FalseValue)
--                                      (BoolBinaryOperations Or (Not FalseValue) FalseValue)

-- tests
hasMainFunction1 :: Test
hasMainFunction1 = TestCase (assertEqual "for hasMainFunction with no main function" False (hasMainFunction functionsWithoutMain))

hasMainFunction2 :: Test
hasMainFunction2 = TestCase (assertEqual "for hasMainFunction with main function with params" False (hasMainFunction functionsWithMainWithParams))

hasMainFunction3 :: Test
hasMainFunction3 = TestCase (assertEqual "for hasMainFunction with valid main" True (hasMainFunction functionsWithValidMain))

-- reduceAnd1 :: Test
-- reduceAnd1 = TestCase (assertEqual "for reduceBoolExp with false and" FalseValue (reduceBoolExp falseAnd))

-- reduceAnd2 :: Test
-- reduceAnd2 = TestCase (assertEqual "for reduceBoolExp with true and" TrueValue (reduceBoolExp trueAnd))

-- reduceAnd3 :: Test
-- reduceAnd3 = TestCase (assertBool "for reduceBoolExp with irreducible and" (FalseValue /= reduceBoolExp dontReduceAnd
--                                                                             && TrueValue /= reduceBoolExp dontReduceAnd))

-- reduceOr1 :: Test
-- reduceOr1 = TestCase (assertEqual "for reduceBoolExp with false or" FalseValue (reduceBoolExp falseOr))

-- reduceOr2 :: Test
-- reduceOr2 = TestCase (assertEqual "for reduceBoolExp with true or" TrueValue (reduceBoolExp trueOr))

-- reduceOr3 :: Test
-- reduceOr3 = TestCase (assertBool "for reduceBoolExp with irreducible or" (FalseValue /= reduceBoolExp dontReduceOr
--                                                                             && TrueValue /= reduceBoolExp dontReduceOr))

-- reduceMixed1 :: Test
-- reduceMixed1 = TestCase (assertEqual "for reduceBoolExp with false mixed" FalseValue (reduceBoolExp mixedFalse))

-- reduceMixed2 :: Test
-- reduceMixed2 = TestCase (assertEqual "for reduceBoolExp with true mixed" TrueValue (reduceBoolExp mixedTrue))


tests :: Test
tests = TestList [TestLabel "hasMainFunction with no main function" hasMainFunction1,
                  TestLabel "hasMainFunction with main function with params" hasMainFunction2,
                  TestLabel "hasMainFunction with main function with valid main" hasMainFunction3,
                  getOptimizerTests]

                --   TestLabel "reduceBoolExp with false and" reduceAnd1,
                --   TestLabel "reduceBoolExp with true and" reduceAnd2,
                --   TestLabel "reduceBoolExp with irreducible and" reduceAnd3,
                --   TestLabel "reduceBoolExp with false or" reduceOr1,
                --   TestLabel "reduceBoolExp with true or" reduceOr2,
                --   TestLabel "reduceBoolExp with irreducible or" reduceOr3,
                --   TestLabel "reduceBoolExp with false mixed" reduceMixed1,
                --   TestLabel "reduceBoolExp with true mixed" reduceMixed2,
                --   TestLabel "reduceArithmeticlExp with simple" reduceSum]


main :: IO Counts
main = runTestTT tests
