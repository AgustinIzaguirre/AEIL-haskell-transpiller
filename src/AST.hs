module AST where

type Name = String

data Program = Root Function
                | Multiple Function Program
                deriving (Show)

data Function = Func Name [Name] Block 
                deriving(Show)

data Block = Empty
            | Actions Statement Block
            | SingleAction Statement
            deriving(Show)

data Statement = Assign Name ValueExp
                | Return ValueExp
                | If BoolExp Block
                | IfElse BoolExp Block Block
                | While BoolExp Block
                | PrintFunc ValueExp
                | FuncCall Name [ValueExp]
                deriving(Show)

data ValueExp = BoolValue BoolExp
                | NumberValue ArithmeticExp
                | StringValue StringExp
                | Apply Name [ValueExp]
                | Var Name
                | Read StringExp
                | GetNumber StringExp
                deriving(Show)


data BoolExp = TrueValue
            | FalseValue
            | BoolVar Name
            | BoolFunc Name [ValueExp]
            | BoolBinaryOperations BoolBinaryOperators BoolExp BoolExp
            | Not BoolExp
            | RelationalBinaryArithmetic RelationalBinaryOperator ArithmeticExp ArithmeticExp
            | RelationalBinaryString RelationalBinaryOperator StringExp StringExp
            deriving(Show)

instance Eq BoolExp where
    (==) TrueValue TrueValue = True
    (==) FalseValue FalseValue = True
    (==) _ _ = False

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
               | NumericVar Name
               | NumericFunc Name [ValueExp]
               | Negate ArithmeticExp
               | ArithmeticBinaryOperation ArithmeticBinaryOperator ArithmeticExp ArithmeticExp
               deriving(Show)

data ArithmeticBinaryOperator = Add
                                | Minus
                                | Multiply
                                | Divide
                                | Modulo
                                | Power
                                deriving(Show)

instance Eq ArithmeticExp where
    (==) (Number number1) (Number number2) = number1 == number2
    (==) _ _ = False


data StringExp = StringConstant String
                | StringVar Name
                | StringFunc Name [ValueExp]
                | StringBinaryOperation StringOperators StringExp StringExp
                deriving(Show)

data StringOperators = Concat
                        deriving(Show)