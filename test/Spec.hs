import Test.HUnit
import Lib
import AST

emptyFunction :: String -> [String] -> Function
emptyFunction name params = Func name params Empty

-- data
functionsWithoutMain :: [FunctionData]
functionsWithoutMain = [("func1",([], emptyFunction "func1" [])), ("func2",([], emptyFunction "func2" []))]

functionsWithMainWithParams :: [FunctionData]
functionsWithMainWithParams = [("func1",([], emptyFunction "func1" [])), ("func2",([], emptyFunction "func2" [])), ("main",(["param1"], emptyFunction "main" ["param1"]))]

functionsWithValidMain :: [FunctionData]
functionsWithValidMain = [("func1",([], emptyFunction "func1" [])), ("func2",([], emptyFunction "func2" [])), ("main",([], emptyFunction "main" []))]

-- tests
hasMainFunction1 :: Test
hasMainFunction1 = TestCase (assertEqual "for hasMainFunction with no main function" False (hasMainFunction functionsWithoutMain))

hasMainFunction2 :: Test
hasMainFunction2 = TestCase (assertEqual "for hasMainFunction with main function with params" False (hasMainFunction functionsWithMainWithParams))

hasMainFunction3 :: Test
hasMainFunction3 = TestCase (assertEqual "for hasMainFunction with valid main" True (hasMainFunction functionsWithValidMain))

tests :: Test
tests = TestList [TestLabel "hasMainFunction with no main function" hasMainFunction1,
                  TestLabel "hasMainFunction with main function with params" hasMainFunction2,
                  TestLabel "hasMainFunction with main function with valid main" hasMainFunction3]


main :: IO Counts
main = runTestTT tests
