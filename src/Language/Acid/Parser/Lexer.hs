module Language.Acid.Parser.Lexer (
	identifier
  , reserved
  , integer, float, charLiteral, stringLiteral
  , parens
  , lexeme, symbol, whiteSpace
) where

import Control.Applicative

import Text.Parsec hiding ((<|>), many)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (GenTokenParser, GenLanguageDef(..), LanguageDef, TokenParser, makeTokenParser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token


language :: LanguageDef st
language = emptyDef {
	identStart      = lower <|> oneOf "+-'*/:,$<>=~#&|@ç^_%!?."
  , identLetter     = identStart language <|> digit
  , reservedNames   = ["define","import","match","lambda"]
  , commentStart    = "/*"
  , commentEnd      = "*/"
  , commentLine     = "//"
  , nestedComments  = True
  , caseSensitive   = True }

lexer :: TokenParser st
lexer = makeTokenParser language


identifier :: Parser String
identifier  = Token.identifier lexer

reserved :: String -> Parser ()
reserved   = Token.reserved   lexer

integer :: Parser Integer
integer = Token.integer lexer

float :: Parser Double
float = Token.float lexer

charLiteral :: Parser Char
charLiteral = Token.charLiteral lexer

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

lexeme :: Parser a -> Parser a
lexeme = Token.lexeme lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer
