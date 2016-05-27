module Language.Acid.Parser (
	parseString
  , parseExpr
  , parseRule
  , parseFile
  , forceRight
  , module Language.Acid.Parser.AST
  , module Language.Acid.Parser.Rules
) where

import Language.Acid.Parser.AST
import Language.Acid.Parser.Lexer
import Language.Acid.Parser.Rules

import Control.Applicative

import Text.Parsec (parse, eof)
import Text.Parsec.String (Parser)
import Text.Parsec.Error (ParseError)


parseString :: String -> Either ParseError Program
parseString = parseRule (program <* eof)

parseExpr :: String -> Either ParseError Expr
parseExpr = parseRule (expr <* eof)

parseRule :: Parser a -> String -> Either ParseError a
parseRule = flip parse "(string)" . (<* eof)

parseFile :: String -> IO (Either ParseError Program)
parseFile = (parseString <$>) . readFile

forceRight :: Show a => Either a b -> b
forceRight x = case x of
	Right res -> res
	Left  err -> error (show err)
