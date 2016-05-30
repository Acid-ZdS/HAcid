module Language.Acid.Interpreter.Types where

import Language.Acid.Parser.AST
import Language.Acid.Interpreter.Environment
import Language.Acid.Types

import Control.Monad.Trans.Except (ExceptT)

import Data.List (intercalate)
import Text.Parsec (ParseError)


data Value
	= IntV Integer
	| FltV Double
	| ChrV Char
	| StrV String
	| UnitV
	| LamV Name Expr (Scope Value)
	| HLam (Value -> ExceptT EvalError IO Value)


data EvalError
	= Mismatch Value Value
	| Undefined Name
	| ImportError FilePath ParseError
	| CustomError String


instance Show Value where
	show (IntV x) = show x
	show (FltV x) = show x
	show (ChrV x) = show x
	show (StrV x) = show x
	show UnitV    = "()"
	show HLam{}   = "<<primitive>>"
	show (LamV param body _) =
			"(lambda (" ++ intercalate " " (reverse params) ++ ") " ++ show ctx ++ ")"
		where
			etaReduce (Lambda p b) ps = etaReduce b (p:ps)
			etaReduce v ps = (ps, v)
			(params, ctx) = etaReduce body [param]


instance Show EvalError where
	show (Mismatch a b) =
		"Mismatch error: failed to apply `" ++ show a ++ "` to value `" ++ show b ++ "`."

	show (Undefined n) =
		"Name error: undefined variable `" ++ n ++ "`."

	show (ImportError path err) =
		"Unable to import " ++ show path ++ ":\n" ++ show err

	show (CustomError str) =
		"Exception: " ++ str
