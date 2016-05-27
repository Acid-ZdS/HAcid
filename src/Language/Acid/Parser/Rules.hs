module Language.Acid.Parser.Rules (
	program

  , statement
  , define, import', tlExpr

  , expr
  , call, lambda, var, lit

  , literal
  , intL, fltL, strL, chrL
) where

import Language.Acid.Parser.AST
import Language.Acid.Parser.Lexer

import Control.Applicative

import Text.Parsec hiding ((<|>), many)
import Text.Parsec.String (Parser)

import Debug.Trace


program :: Parser Program
program = whiteSpace >> Program <$> many (lexeme statement)
	   <?> "program"

statement               :: Parser Statement
define, import', tlExpr :: Parser Statement

statement =  try tlExpr
		 <|> try define
         <|> import'
		 <?> "statement"

define  = parens (reserved "define" >> Define <$> lexeme identifier <*> expr)
import' = parens (reserved "import" >> Import <$> identifier)
tlExpr  = TLExpr <$> expr

expr :: Parser Expr
call, lambda, lit, var :: Parser Expr

expr =  try lambda
	<|> try call
	<|> try (parens expr)
	<|> var
	<|> lit
	<?> "expression"

var    = Variable <$> identifier
lit    = Literal <$> literal
call   = parens (foldl1 Call <$> many1 (lexeme expr))
lambda = parens $ do
	reserved "lambda"
	params <- parens (many1 (lexeme identifier))
	body <- expr
	return (foldr Lambda body params)


literal                :: Parser Literal
intL, fltL, strL, chrL :: Parser Literal

literal = try fltL <|> intL <|> strL <|> chrL <?> "literal"
intL = IntL <$> integer
fltL = FltL <$> float
strL = StrL <$> stringLiteral
chrL = ChrL <$> charLiteral
