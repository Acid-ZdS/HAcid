module Language.Acid.Parser.AST (
	Program(..)
  , Statement(..)
  , Expr(..)
  , Literal(..)
  , ModPath
) where

import Language.Acid.Types

import Data.List (intercalate)

type ModPath = [String]

data Program = Program {
	filepath :: Maybe FilePath
 ,  instrs   :: [Statement]
}


data Statement
	= Define Name Expr
	| Import ModPath
	| TLExpr Expr

data Expr
	= Lambda Name Expr
	| Call Expr [Expr]
	| Variable Name
	| Literal Literal

data Literal
	= IntL Integer
	| FltL Double
	| StrL String
	| ChrL Char
	| UnitL


instance Show Program where
	show (Program _ instrs) = unlines (map show instrs)


instance Show Statement where
	show (Define name expr) = show (Call (Variable "define") [Variable name, expr])
	show (TLExpr expr)      = show expr
	show (Import path)      =
		"(import " ++ intercalate "." path ++ ")"


instance Show Expr where
	show (Variable n) = n
	show (Literal x) = show x
	show (Call f args) =
			"(" ++ show f ++ concatMap (' ':) (map show args) ++ ")"
	show (Lambda param body) =
			"(lambda (" ++ intercalate " " (reverse params) ++ ") " ++ show ctx ++ ")"
		where
			etaReduce (Lambda p b) ps = etaReduce b (p:ps)
			etaReduce v ps = (ps, v)
			(params, ctx) = etaReduce body [param]


instance Show Literal where
	show (IntL x) = show x
	show (FltL x) = show x
	show (ChrL x) = show x
	show (StrL x) = show x
