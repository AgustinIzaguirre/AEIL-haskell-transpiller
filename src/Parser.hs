module Parser where

import Text.Parsec (char, try, (<|>))
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
    (Program(Root, Multiple), Function(Func),  Name, StringExp(StringConstant),  RelationalBinaryOperator(GreaterOrEqual, Greater, LessOrEqual, Less, NotEquals, Equals),  Statement(While, PrintFunc, IfElse, Assign, Return), Block(Empty, Actions, SingleAction), ValueExp(NumberValue, BoolValue),  Statement(If),  ArithmeticBinaryOperator(Modulo, Minus, Add, Multiply, Divide),
      ArithmeticExp(Number, ArithmeticBinaryOperation, Negate),
      BoolBinaryOperators(Or, And),
      BoolExp(FalseValue, TrueValue, BoolBinaryOperations, Not, RelationalBinaryArithmetic),
      Program )

arithmeticOperators :: [[Expr.Operator String () Data.Functor.Identity.Identity ArithmeticExp]]
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

booleanOperators :: [[Expr.Operator String () Data.Functor.Identity.Identity BoolExp]]
booleanOperators = 
    [
        [ Expr.Prefix (reservedOperators "!" >> return Not) ],
        [ Expr.Infix (reservedOperators "&&" >> return (BoolBinaryOperations And )) Expr.AssocLeft,
          Expr.Infix (reservedOperators "||" >> return (BoolBinaryOperations Or )) Expr.AssocLeft ]
    ]

-- TODO check that after parsing everything there is nothing left or spaces or comments
parseFile :: Parser Program
parseFile = whiteSpace >> program

program :: Parser Program 
program = try multipleFunctionsProgram
        <|> try singleFunctionProgram

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
    funcArguments <- parenthesis arguments
    funcBlock <- braces block
    return (Func funcName funcArguments funcBlock)

argument :: Parser Name
argument = identifier

arguments :: Parser [Name]
arguments = commaSeparated argument

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

stringExpression :: Parser StringExp
stringExpression = string >>= \text -> return (StringConstant text)
                    -- <|> stringOperation  

booleanExpression :: Parser BoolExp
booleanExpression = Expr.buildExpressionParser booleanOperators boolean

boolean :: Parser BoolExp 
boolean = try (parenthesis booleanExpression)
            <|> try (reserved "true" >> return TrueValue)
            <|> try (reserved "false" >> return FalseValue)
            <|> try realtionalExpression

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
        -- TODO with variables and constructor <|> fmap Name identifier

valueExpression :: Parser ValueExp 
valueExpression = parenthesis valueExpression
                <|> (booleanExpression >>= \value -> return (BoolValue value))
                <|> (arithmeticExpression >>= \value -> return (NumberValue value))

realtionalExpression :: Parser BoolExp
realtionalExpression = arithmeticRelation
                    -- TODO <|> stringRelation

arithmeticRelation :: Parser BoolExp
arithmeticRelation = do
                first <- arithmeticExpression
                operator <- relationalBinaryOperator
                second <- arithmeticExpression
                return (RelationalBinaryArithmetic operator first second)

relationalBinaryOperator :: Parser RelationalBinaryOperator
relationalBinaryOperator = (reservedOperators "==" >> return Equals)
                        <|> (reservedOperators "!=" >> return NotEquals)
                        <|> (reservedOperators "<" >> return Less)
                        <|> (reservedOperators "<=" >> return LessOrEqual)
                        <|> (reservedOperators ">" >> return Greater)
                        <|> (reservedOperators ">=" >> return GreaterOrEqual)

