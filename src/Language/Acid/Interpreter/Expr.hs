module Language.Acid.Interpreter.Expr where

import Language.Acid.Parser.AST
import Language.Acid.Interpreter.Types
import Language.Acid.Interpreter.Environment

import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Except (throwError)


evalExpr :: Expr -> Scope Value -> ExceptT EvalError IO Value
evalExpr (Lambda n e) env = return (LamV n e env)
evalExpr (Call f x) env = do
		arg <- evalExpr x env
		fv  <- evalExpr f env
		case fv of
			LamV n e c -> evalExpr e (declare n arg c)
			HLam fn    -> fn arg
			notLam     -> throwError (Mismatch arg notLam)
evalExpr (Variable n) env =
	case fetch n env of
		Just x  -> return x
		Nothing -> throwError (Undefined n)
evalExpr (Literal lit) _ = return (evalLit lit)


evalLit :: Literal -> Value
evalLit (IntL x) = IntV x
evalLit (FltL x) = FltV x
evalLit (ChrL x) = ChrV x
evalLit (StrL x) = StrV x
evalLit UnitL    = UnitV
