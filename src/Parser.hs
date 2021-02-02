module Parser where

import Text.Parsec (getInput, char, try, (<|>))
import Text.Parsec.String (Parser)
import Text.Parsec.Prim (many)
import qualified Text.Parsec.Expr as Expr
import qualified Control.Monad.Identity as Data.Functor.Identity
import qualified Text.Parsec as Text.Parsec.Prim

import Lexer
    (commaSeparated, string,  integer,
      parenthesis,
      braces,
      semiColon,
      identifier,
      reserved,
      reservedOperators,
      whiteSpace )
import AST
    (StringOperators(Concat), Program(Root, Multiple), Function(Func),  Name, StringExp(StringVar, StringBinaryOperation, StringConstant),
      RelationalBinaryOperator(GreaterOrEqual, Greater, LessOrEqual, Less, NotEquals, Equals),
      Statement(While, PrintFunc, IfElse, Assign, Return, FuncCall), Block(Empty, Actions, SingleAction),
      ValueExp(StringValue, Read, Var, Apply, NumberValue, BoolValue),  Statement(If),
        ArithmeticBinaryOperator(Power, Modulo, Minus, Add, Multiply, Divide),
      ArithmeticExp(NumericVar, Number, ArithmeticBinaryOperation, Negate),
      BoolBinaryOperators(Or, And),
      BoolExp(BoolVar, RelationalBinaryString, FalseValue, TrueValue, BoolBinaryOperations, Not, RelationalBinaryArithmetic),
      Program )
import Debug.Trace

arithmeticOperators :: [[Expr.Operator String () Data.Functor.Identity.Identity ArithmeticExp]]
arithmeticOperators =
            [
                [ Expr.Infix (reservedOperators "**" >> return (ArithmeticBinaryOperation Power )) Expr.AssocRight ],
                [ Expr.Prefix (reservedOperators "-" >> return Negate) ],
                [ Expr.Infix (reservedOperators "*" >> return (ArithmeticBinaryOperation Multiply )) Expr.AssocLeft,
                  Expr.Infix (reservedOperators "/" >> return (ArithmeticBinaryOperation Divide )) Expr.AssocLeft,
                  Expr.Infix (reservedOperators "%" >> return (ArithmeticBinaryOperation Modulo )) Expr.AssocLeft ],
                [ Expr.Infix (reservedOperators "+" >> return (ArithmeticBinaryOperation Add )) Expr.AssocLeft,
                  Expr.Infix (reservedOperators "-" >> return (ArithmeticBinaryOperation Minus )) Expr.AssocLeft ]
            ]

booleanOperators :: [[Expr.Operator String () Data.Functor.Identity.Identity BoolExp]]
booleanOperators = 
    [
        [ Expr.Prefix (reservedOperators "!" >> return Not) ],
        [ Expr.Infix (reservedOperators "&&" >> return (BoolBinaryOperations And )) Expr.AssocLeft,
          Expr.Infix (reservedOperators "||" >> return (BoolBinaryOperations Or )) Expr.AssocLeft ]
    ]

stringOperators :: [[Expr.Operator String () Data.Functor.Identity.Identity StringExp]]
stringOperators =
    [
        [ Expr.Infix (reservedOperators "++" >> return (StringBinaryOperation Concat )) Expr.AssocLeft ]
    ]

parseFile :: Parser Program
parseFile = whiteSpace >> program 

program :: Parser Program 
program = try multipleFunctionsProgram
        <|> singleFunctionProgram

singleFunctionProgram :: Parser Program
singleFunctionProgram = do 
                func <- function
                return (Root func)

multipleFunctionsProgram :: Parser Program
multipleFunctionsProgram = do
                    func <- function
                    nextFunctions <- program
                    return (Multiple func nextFunctions)

function :: Parser Function
function = do
    reserved "func"
    funcName <- identifier
    funcParameters <- parenthesis parameters
    funcBlock <- braces block
    return (Func funcName funcParameters funcBlock)

parameter :: Parser Name
parameter = identifier

parameters :: Parser [Name]
parameters = commaSeparated parameter

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
statement = try funcCallStatement
            <|> try assignStatement
            <|> try returnStatement
            <|> try ifElseStatement
            <|> try ifStatement
            <|> try whileStatement
            <|> try printFuncStatement

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

printFuncStatement :: Parser Statement
printFuncStatement = do
    reserved "print"
    text <- parenthesis stringExpression
    semiColon
    return (PrintFunc text)

funcCallStatement :: Parser Statement
funcCallStatement = do
    funcName <- identifier
    funcArguments <- parenthesis arguments
    semiColon
    return (FuncCall funcName funcArguments)

arguments :: Parser [ValueExp]
arguments = commaSeparated valueExpression

stringExpression :: Parser StringExp
stringExpression = Expr.buildExpressionParser stringOperators stringPossibleValues
                    -- <|> Expr.buildExpressionParser stringOperators (identifier >>= \name -> return (StringVar name))

stringPossibleValues :: Parser StringExp
stringPossibleValues = stringValue -- try (identifier >>= \name -> return (StringVar name))
                        -- <|> stringValue

stringValue :: Parser StringExp
stringValue = parenthesis stringExpression
            <|> (string >>= \text -> return (StringConstant text))
            -- <|> try (identifier >>= \varName -> return (StringVar varName))

booleanExpression :: Parser BoolExp
booleanExpression = Expr.buildExpressionParser booleanOperators booleanPossibleValues
                    -- <|> Expr.buildExpressionParser booleanOperators (identifier >>= \name -> return (BoolVar name))

booleanPossibleValues :: Parser BoolExp
booleanPossibleValues = boolean

boolean :: Parser BoolExp 
boolean = try realtionalExpression
            <|> try (parenthesis booleanExpression)
            <|> try (reserved "true" >> return TrueValue)
            <|> try (reserved "false" >> return FalseValue)
            <|> (identifier >>= \varName -> return (BoolVar varName))

returnStatement :: Parser Statement
returnStatement = do
    reserved "return"
    value <- valueExpression
    semiColon 
    return (Return value)

arithmeticExpression :: Parser ArithmeticExp 
arithmeticExpression = Expr.buildExpressionParser arithmeticOperators numericPossibleValues

numericPossibleValues :: Parser ArithmeticExp
numericPossibleValues = try (identifier >>= \name -> return (NumericVar name))
                        <|> number

number :: Parser ArithmeticExp 
number = try (fmap Number integer)
        <|> try (parenthesis arithmeticExpression)
        -- <|> try (identifier >>= \varName -> return (NumericVar varName))

valueExpression :: Parser ValueExp 
valueExpression =  try applyFunc
                <|> try (arithmeticExpression >>= \value -> return (NumberValue value))
                <|> try (booleanExpression >>= \value -> return (BoolValue value))
                <|> try (stringExpression >>= \value -> return (StringValue value))
                <|> try (identifier >>= \varName -> return (Var varName))
                <|> readExpression

applyFunc :: Parser ValueExp
applyFunc = do
    funcName <- identifier
    funcArguments <- parenthesis arguments
    return (Apply funcName funcArguments)

readExpression :: Parser ValueExp
readExpression = do
        reserved "read"
        promptText <- parenthesis stringExpression
        return (Read promptText)

realtionalExpression :: Parser BoolExp
realtionalExpression = try arithmeticRelation
                    <|> try stringRelation

arithmeticRelation :: Parser BoolExp
arithmeticRelation = do
                first <- arithmeticExpression
                operator <- relationalBinaryOperator
                second <- arithmeticExpression
                return (RelationalBinaryArithmetic operator first second)

stringRelation :: Parser BoolExp
stringRelation = do
                first <- stringExpression
                operator <- relationalBinaryOperator
                second <- stringExpression
                return (RelationalBinaryString operator first second)

relationalBinaryOperator :: Parser RelationalBinaryOperator
relationalBinaryOperator = (reservedOperators "==" >> return Equals)
                        <|> (reservedOperators "!=" >> return NotEquals)
                        <|> (reservedOperators "<" >> return Less)
                        <|> (reservedOperators "<=" >> return LessOrEqual)
                        <|> (reservedOperators ">" >> return Greater)
                        <|> (reservedOperators ">=" >> return GreaterOrEqual)

