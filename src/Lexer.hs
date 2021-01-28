module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token
import qualified Control.Monad.Identity as Data.Functor.Identity
import qualified Text.Parsec as Text.Parsec.Prim

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
    where
        operations = ["+", "*", "-", ";", "\\", "%", "**", "&&", "||", "!", "++", "=", "==", "!="]
        reservedNames = ["func", "if", "else", "while", "true", "false"]
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

braces :: Parser a -> Parser a
braces = Token.braces lexer

commaSeparated :: Parser a -> Parser [a]
commaSeparated = Token.commaSep lexer

semiColonSeparated :: Parser a -> Parser [a]
semiColonSeparated = Token.semiSep lexer

semiColon :: Text.Parsec.Prim.ParsecT String () Data.Functor.Identity.Identity String
semiColon = Token.semi lexer

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOperators :: String -> Parser ()
reservedOperators = Token.reservedOp lexer

whiteSpace :: Text.Parsec.Prim.ParsecT String () Data.Functor.Identity.Identity ()
whiteSpace = Token.whiteSpace lexer