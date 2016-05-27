module Language.Acid.Parser.AST (
	Program(..)
  , Statement(..)
  , Expr(..)
  , Literal(..)
) where

import Language.Acid.Types


newtype Program = Program [Statement]
	deriving Show


data Statement
	= Define Name Expr
	| Import Name
	| TLExpr Expr
	deriving Show

data Expr
	= Lambda Name Expr
	| Call Expr Expr
	| Variable Name
	| Literal Literal
	deriving Show

data Literal
	= IntL Integer
	| FltL Double
	| StrL String
	| ChrL Char
	deriving Show
