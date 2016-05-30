module Language.Acid.Interpreter.Expr where

import Language.Acid.Parser.AST
import Language.Acid.Interpreter.Types
import Language.Acid.Interpreter.Environment

import Control.Applicative
import Control.Monad (mapM)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Except (throwError)


evalExpr :: Expr -> Scope Value -> ExceptT EvalError IO Value
evalExpr (Lambda n e) env = return (LamV n e env)
evalExpr (Call f args) env = do
		args <- mapM (flip evalExpr env) args
		fn   <- evalExpr f env
		betaReduceMany args fn
evalExpr (Variable n) env =
	case fetch n env of
		Just x  -> return x
		Nothing -> throwError (Undefined n)
evalExpr (Literal lit) _ = return (evalLit lit)


betaReduceMany :: [Value] -> Value -> ExceptT EvalError IO Value
betaReduceMany []  (Variadic acc minArity fn) =
	return $ if minArity <= 0 then
			 	 acc
			 else
				 Variadic acc (minArity - 1) fn
betaReduceMany []  fn = return fn
betaReduceMany (x:xs) fn = do
	newVal <- betaReduce x fn
	betaReduceMany xs newVal

betaReduce :: Value -> Value -> ExceptT EvalError IO Value
betaReduce x (Variadic acc minArity fn) =
	Variadic <$> fn acc x <*> pure (minArity - 1) <*> pure fn
betaReduce x (LamV n e c)      = evalExpr e (declare n x c)
betaReduce x (HLam fn)         = fn x
betaReduce arg notLam          = throwError (Mismatch arg notLam)

evalLit :: Literal -> Value
evalLit (IntL x) = IntV x
evalLit (FltL x) = FltV x
evalLit (ChrL x) = ChrV x
evalLit (StrL x) = StrV x
evalLit UnitL    = UnitV
