module Parser where

import Text.Parsec (sepBy1, (<|>))
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Expr

import Lexer
import AST
    (Statement(Return, Block), ValueExp(BoolValue),  Statement(If),  ArithmeticBinaryOperator(Modulo, Minus, Add, Multiply, Divide),
      ArithmeticExp(ArithmeticBinaryOperation, Negate),
      BoolBinaryOperators(Or, And),
      BoolExp(FalseValue, TrueValue, BoolBinaryOperations, Not),
      Program )
import qualified Control.Monad.Identity as Data.Functor.Identity
import qualified Text.Parsec as Text.Parsec.Prim

arithmeticOperators =
            [
                -- TODO add pow here maybe
                [ Expr.Prefix (reservedOperators "-"   >> return Negate) ],
                [ Expr.Infix (reservedOperators "*" >> return (ArithmeticBinaryOperation Multiply )) Expr.AssocLeft,
                  Expr.Infix (reservedOperators "/" >> return (ArithmeticBinaryOperation Divide )) Expr.AssocLeft,
                  Expr.Infix (reservedOperators "%" >> return (ArithmeticBinaryOperation Modulo )) Expr.AssocLeft ],
                [ Expr.Infix (reservedOperators "+" >> return (ArithmeticBinaryOperation Add )) Expr.AssocLeft,
                  Expr.Infix (reservedOperators "-" >> return (ArithmeticBinaryOperation Minus )) Expr.AssocLeft ]
            ]

booleanOperators = 
    [
        [ Expr.Prefix (reservedOperators "!"   >> return Not) ],
        [ Expr.Infix (reservedOperators "&&" >> return (BoolBinaryOperations And )) Expr.AssocLeft,
          Expr.Infix (reservedOperators "||" >> return (BoolBinaryOperations Or )) Expr.AssocLeft ]
    ]

parseFile :: Parser Statement
parseFile = whiteSpace >> block

block :: Parser Statement
block = parenthesis block
        <|> statementList

statementList :: Parser Statement
statementList = do 
    statements <- sepBy1 statement semiColon
    return (if length statements == 1 then head statements else Block statements)

statement :: Parser Statement 
statement = ifStatement
            <|> returnStatement

ifStatement :: Parser Statement
ifStatement = do
    reserved "if"
    condition <- parenthesis booleanExpression
    ifBlock <- braces statement
    return (If condition ifBlock)

booleanExpression :: Parser BoolExp
booleanExpression = Expr.buildExpressionParser booleanOperators boolean

boolean :: Parser BoolExp 
boolean = parenthesis booleanExpression
            <|> (reserved "true" >> return TrueValue)
            <|> (reserved "false" >> return FalseValue)

returnStatement :: Parser Statement
returnStatement = do
    reserved "return"
    value <- boolean
    semiColon 
    return (Return (BoolValue value))

