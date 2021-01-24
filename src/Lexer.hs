module Lexer where
import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))

import Control.Applicative

import Control.Monad.Identity (Identity)

parseH :: IO ()
parseH = print ( Parsec.parse (Parsec.char 'H') "(source)" "Hola")