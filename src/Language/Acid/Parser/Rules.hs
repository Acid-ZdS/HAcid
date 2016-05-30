module Language.Acid.Parser.Rules (
	program, programFile

  , statement
  , define, import', tlExpr
  , modPath

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


programFile :: String -> Parser Program
programFile path = whiteSpace >> Program (Just path) <$> many (lexeme statement)
	            <?> "program"

program :: Parser Program
program = whiteSpace >> Program Nothing <$> many (lexeme statement)
 	   <?> "program"

statement               :: Parser Statement
define, import', tlExpr :: Parser Statement

statement =  try tlExpr
		 <|> try define
         <|> import'
		 <?> "statement"

define  = parens (reserved "define" >> Define <$> lexeme identifier <*> expr)
import' = parens (reserved "import" >> Import <$> modPath)
tlExpr  = TLExpr <$> expr

modPath :: Parser ModPath
modPath = sepBy1 identifier (char '.')

expr :: Parser Expr
call, lambda, lit, var :: Parser Expr

expr =  try lambda
	<|> try call
	<|> var
	<|> lit
	<?> "expression"

var    = Variable <$> identifier
lit    = Literal <$> literal
call   = parens (Call <$> lexeme expr <*> many (lexeme expr))
lambda = parens $ do
	reserved "lambda"
	params <- parens (many1 (lexeme identifier))
	body <- expr
	return (foldr Lambda body params)


literal                       :: Parser Literal
intL, fltL, strL, chrL, unitL :: Parser Literal

literal = try fltL <|> intL <|> strL <|> chrL <|> unitL <?> "literal"
intL = IntL <$> integer
fltL = FltL <$> float
strL = StrL <$> stringLiteral
chrL = ChrL <$> charLiteral
unitL = symbol "()" >> return UnitL
