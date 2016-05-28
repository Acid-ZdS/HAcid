module Language.Acid.Parser.AST (
	Program(..)
  , Statement(..)
  , Expr(..)
  , Literal(..)
) where

import Language.Acid.Types

import Data.List (intercalate)


newtype Program = Program [Statement]


data Statement
	= Define Name Expr
	| Import Name
	| TLExpr Expr

data Expr
	= Lambda Name Expr
	| Call Expr Expr
	| Variable Name
	| Literal Literal

data Literal
	= IntL Integer
	| FltL Double
	| StrL String
	| ChrL Char


instance Show Program where
	show (Program instrs) = unlines (map show instrs)


instance Show Statement where
	show (Define name expr) = show (Call (Variable "define") expr)
	show (Import name)      = show (Call (Variable "import") (Variable name))
	show (TLExpr expr)      = show expr


instance Show Expr where
	show (Variable n) = n
	show (Literal x) = show x
	show (Call f x) =
			"(" ++ show fn ++ concatMap (' ':) (map show args) ++ ")"
		where
			betaReduce (Call g arg) args = betaReduce g (arg:args)
			betaReduce fn args = (fn, args)
			(fn, args) = betaReduce f [x]
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
