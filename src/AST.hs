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
        | AND BoolExp BoolExp
        | OR BoolExp BoolExp
        | NOTOP BoolExp
        -- | ArithmeticExp == ArithmeticExp
        -- | ArithmeticExp != ArithmeticExp
        -- | ArithmeticExp < ArithmeticExp
        -- | ArithmeticExp <= ArithmeticExp
        -- | ArithmeticExp > ArithmeticExp
        -- | ArithmeticExp >= ArithmeticExp
        -- | StringExp == StringExp
        -- | StringExp != StringExp
        -- | StringExp < StringExp
        -- | StringExp <= StringExp
        -- | StringExp > StringExp
        -- | StringExp >= StringExp
        -- TODO

data ArithmeticExp = Number Integer
               | NumVar Name
               | NumFunc FuncCall
               | Add ArithmeticExp ArithmeticExp
               | Minus ArithmeticExp ArithmeticExp
               | Negate ArithmeticExp
               | Multiply ArithmeticExp ArithmeticExp
               | Divide ArithmeticExp ArithmeticExp
               | Modulo ArithmeticExp ArithmeticExp

data StringExp = StringValue String
                | StringVar Name
                | StringFunc FuncCall
                -- | StringExp CONCAT StringExp
                -- TODO            
