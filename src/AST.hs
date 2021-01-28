module AST where

type Name = String

data Program = Root Function
                | Multiple Function Program
                deriving (Show)

data Function = Name [Name] Block 
                deriving(Show)

data Block = Empty
                | Actions Statement Block
                deriving(Show)
                -- | ConditionalBlock Block
                -- | LoopBlock Block
                -- TODO

data Statement = Asign Name ValueExp
                | Return ValueExp
                deriving(Show)
                -- | PrintFunc
                -- TODO

data ValueExp = BoolExp
                | ArithmeticExp
                | StringExp
                | APPLY FuncCall
                deriving(Show)

data FuncCall = Call Name [ValueExp]
                deriving(Show)

data BoolExp = True
            | False
            | BoolVar Name
            | BoolFunc FuncCall
            | BoolBinaryOperations BoolBinaryOperators BoolExp BoolExp
            | Not BoolExp
            | RelationalBinaryArithmetic RelationalBinaryOperator ArithmeticExp ArithmeticExp
            | RelationalBinaryString RelationalBinaryOperator StringExp StringExp
            deriving(Show)

data RelationalBinaryOperator = Equals
                                | NotEquals
                                | Less
                                | LessOrEqual
                                | Greater
                                | GreaterOrEqual
                                deriving(Show)
data BoolBinaryOperators = And
                        | Or
                        deriving(Show)

data ArithmeticExp = Number Integer
               | NumVar Name
               | NumFunc FuncCall
               | Negate ArithmeticExp
               | ArithmeticBinaryOperation ArithmeticBinaryOperator ArithmeticExp ArithmeticExp
               deriving(Show)

data ArithmeticBinaryOperator = Add
                                | Minus
                                | Multiply
                                | Divide
                                | Modulo
                                deriving(Show)


data StringExp = StringValue String
                | StringVar Name
                | StringFunc FuncCall
                | StringBinaryOperation StringOperators StringExp StringExp
                deriving(Show)

data StringOperators = Concat
                        deriving(Show)