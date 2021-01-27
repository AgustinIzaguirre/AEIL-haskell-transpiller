module AST where

type Name = String

data Program = Root Function
                | Multiple Function Program

data Function = Name [Name] Block

data Block = Empty
                | Actions Statement Block
                -- | ConditionalBlock Block
                -- | LoopBlock Block
                -- TODO

data Statement = Asign Name ValueExp
                | Return ValueExp
                -- | PrintFunc
                --  TODO

data ValueExp = BoolExp
                | ArithmeticExp
                | StringExp
                | APPLY FuncCall

data FuncCall = Call Name [ValueExp]

data BoolExp = True
            | False
            | BoolVar Name
            | BoolFunc FuncCall
            | BoolBinaryOperations BoolBinaryOperators BoolExp BoolExp
            | Not BoolExp
            | RelationalBinaryArithmetic RelationalBinaryOperator ArithmeticExp ArithmeticExp
            | RelationalBinaryString RelationalBinaryOperator StringExp StringExp

data RelationalBinaryOperator = Equals
                                | NotEquals
                                | Less
                                | LessOrEqual
                                | Greater
                                | GreaterOrEqual
data BoolBinaryOperators = And
                        | Or

data ArithmeticExp = Number Integer
               | NumVar Name
               | NumFunc FuncCall
               | Negate ArithmeticExp
               | ArithmeticBinaryOperation ArithmeticBinaryOperator ArithmeticExp ArithmeticExp

data ArithmeticBinaryOperator = Add
                                | Minus
                                | Multiply
                                | Divide
                                | Modulo


data StringExp = StringValue String
                | StringVar Name
                | StringFunc FuncCall
                | StringBinaryOperation StringOperators StringExp StringExp

data StringOperators = Concat