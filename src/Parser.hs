module Parser where

import Text.Parsec (try, (<|>))
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Expr
import qualified Control.Monad.Identity as Data.Functor.Identity
import qualified Text.Parsec as Text.Parsec.Prim

import Lexer
    ( integer,
      parenthesis,
      braces,
      semiColon,
      identifier,
      reserved,
      reservedOperators,
      whiteSpace )
import AST
    (Statement(While, PrintFunc, IfElse, Assign, Return), Block(Empty, Actions, SingleAction), ValueExp(NumberValue, BoolValue),  Statement(If),  ArithmeticBinaryOperator(Modulo, Minus, Add, Multiply, Divide),
      ArithmeticExp(Number, ArithmeticBinaryOperation, Negate),
      BoolBinaryOperators(Or, And),
      BoolExp(FalseValue, TrueValue, BoolBinaryOperations, Not),
      Program )

arithmeticOperators =
            [
                -- TODO add pow here maybe
                [ Expr.Prefix (reservedOperators "-" >> return Negate) ],
                [ Expr.Infix (reservedOperators "*" >> return (ArithmeticBinaryOperation Multiply )) Expr.AssocLeft,
                  Expr.Infix (reservedOperators "/" >> return (ArithmeticBinaryOperation Divide )) Expr.AssocLeft,
                  Expr.Infix (reservedOperators "%" >> return (ArithmeticBinaryOperation Modulo )) Expr.AssocLeft ],
                [ Expr.Infix (reservedOperators "+" >> return (ArithmeticBinaryOperation Add )) Expr.AssocLeft,
                  Expr.Infix (reservedOperators "-" >> return (ArithmeticBinaryOperation Minus )) Expr.AssocLeft ]
            ]

booleanOperators = 
    [
        [ Expr.Prefix (reservedOperators "!" >> return Not) ],
        [ Expr.Infix (reservedOperators "&&" >> return (BoolBinaryOperations And )) Expr.AssocLeft,
          Expr.Infix (reservedOperators "||" >> return (BoolBinaryOperations Or )) Expr.AssocLeft ]
    ]

parseFile :: Parser Block
parseFile = whiteSpace >> block

block :: Parser Block 
block = emptyBlock
        <|> try multipleStatementBlock
        <|> try singleStatementBlock

emptyBlock :: Parser Block
emptyBlock = do
        semiColon
        return Empty
    
singleStatementBlock :: Parser Block
singleStatementBlock = do 
                action <- statement
                return (SingleAction action)

multipleStatementBlock :: Parser Block
multipleStatementBlock = do
                    action <- statement
                    nextActions <- block
                    return (Actions action nextActions)

statement :: Parser Statement 
statement = try assignStatement
            <|> try returnStatement
            <|> try ifElseStatement
            <|> try ifStatement
            <|> try whileStatement
            -- <|> printFuncStatement

assignStatement :: Parser Statement
assignStatement = do
    name <- identifier
    reservedOperators "="
    value <- valueExpression
    semiColon
    return (Assign name value)

ifStatement :: Parser Statement
ifStatement = do
    reserved "if"
    condition <- parenthesis booleanExpression
    ifBlock <- braces block
    return (If condition ifBlock)

ifElseStatement :: Parser Statement
ifElseStatement = do
    reserved "if"
    ifCondition <- parenthesis booleanExpression
    ifBlock <- braces block
    reserved "else"
    elseBlock <- braces block
    return (IfElse ifCondition ifBlock elseBlock)

whileStatement :: Parser Statement
whileStatement = do
    reserved "while"
    condition <- parenthesis booleanExpression
    whileBlock <- braces block
    return (While condition whileBlock)

-- TODO and fix
-- printFuncStatement :: Parser Statement
-- printFuncStatement = do
--     reserved "print"
--     text <- string
--     return (PrintFunc text)

booleanExpression :: Parser BoolExp
booleanExpression = Expr.buildExpressionParser booleanOperators boolean

boolean :: Parser BoolExp 
boolean = parenthesis booleanExpression
            <|> (reserved "true" >> return TrueValue)
            <|> (reserved "false" >> return FalseValue)

returnStatement :: Parser Statement
returnStatement = do
    reserved "return"
    value <- valueExpression
    semiColon 
    return (Return value)

arithmeticExpression :: Parser ArithmeticExp 
arithmeticExpression = Expr.buildExpressionParser arithmeticOperators number

number :: Parser ArithmeticExp 
number = parenthesis arithmeticExpression
        <|> fmap Number integer
        -- TODO with variables and constructor <|> fmap Var identifier

-- number = parenthesis arithmeticExpression
--         <|> (integer >>= \value -> return (Number value))

valueExpression :: Parser ValueExp 
valueExpression = parenthesis valueExpression
                <|> (booleanExpression >>= \value -> return (BoolValue value))
                <|> (arithmeticExpression >>= \value -> return (NumberValue value))


