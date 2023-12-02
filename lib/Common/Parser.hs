module Common.Parser where

import Data.Functor.Identity (Identity)
import Text.Parsec.Language (haskellStyle)
import qualified Text.Parsec.Token as T
import Text.ParserCombinators.Parsec

lexer :: T.GenTokenParser String u Identity
lexer =
  T.makeTokenParser
    ( haskellStyle
        { T.identStart = letter <|> char '_',
          T.reservedNames = ["let", "assume", "putStrLn", "print", "out", "case", "of", "forall"],
          T.reservedOpNames = ["\\", "->", "::", "=", "."]
        }
    )

whitespace :: Parser ()
whitespace = T.whiteSpace lexer

identifier :: Parser String
identifier = T.identifier lexer

reserved :: String -> Parser ()
reserved = T.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = T.reservedOp lexer

operator :: Parser String
operator = T.operator lexer

parens :: Parser a -> Parser a
parens = T.parens lexer

brackets :: Parser a -> Parser a
brackets = T.brackets lexer

braces :: Parser a -> Parser a
braces = T.braces lexer

commaSep :: Parser a -> Parser [a]
commaSep = T.commaSep lexer

commaSep1 :: Parser a -> Parser [a]
commaSep1 = T.commaSep1 lexer

symbol :: String -> Parser String
symbol = T.symbol lexer

stringLiteral :: Parser String
stringLiteral = T.stringLiteral lexer

charLiteral :: Parser Char
charLiteral = T.charLiteral lexer

intLiteral :: Parser Integer
intLiteral = T.integer lexer

natLiteral :: Parser Integer
natLiteral = T.natural lexer

comma :: Parser String
comma = T.comma lexer
