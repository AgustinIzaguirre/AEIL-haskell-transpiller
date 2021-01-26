module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
    where
        operations = ["+","*","-",";"]
        reservedNames = ["def","extern"]
        -- operations = ["+", "*", "-", ";", "\\", "%", "**", "&&", "||", "!", "++"]
        -- reservedNames = ["func", "if", "while"]
        style = emptyDef {
                Token.commentLine = "#",
                Token.reservedOpNames = operations,
                Token.reservedNames = reservedNames,
                Token.commentStart = "\\*",
                Token.commentEnd = "*/"
            }

integer :: Parser Integer
integer = Token.integer lexer

parenthesis :: Parser a -> Parser a
parenthesis = Token.parens lexer

brackets :: Parser a -> Parser a
brackets = Token.brackets lexer

commaSeparated :: Parser a -> Parser [a]
commaSeparated = Token.commaSep lexer

semiColonSeparated :: Parser a -> Parser [a]
semiColonSeparated = Token.semiSep lexer

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOperators :: String -> Parser ()
reservedOperators = Token.reservedOp lexer