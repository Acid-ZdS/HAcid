module Language.Acid.Interpreter.Program where

import Language.Acid.Parser
import Language.Acid.Interpreter.Expr
import Language.Acid.Interpreter.Environment
import Language.Acid.Interpreter.Types

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Except (throwError)

import Data.List (intercalate)
import qualified Data.Map as Map

evalProgram :: Program -> Scope Value -> ExceptT EvalError IO (Scope Value)
evalProgram (Program instrs) env = evalStmtList instrs env

evalStmtList :: [Statement] -> Scope Value -> ExceptT EvalError IO (Scope Value)
evalStmtList [] env = return env
evalStmtList (instr:instrs) env = do
	newEnv <- evalStmt instr env
	evalStmtList instrs newEnv

evalStmt :: Statement -> Scope Value -> ExceptT EvalError IO (Scope Value)
evalStmt (Define name aval) env = do
	val <- evalExpr aval env
	return (declare name val env)
evalStmt (TLExpr expr) env = evalExpr expr env >> return env
evalStmt (Import modpath) env = do
	let filepath = intercalate "/" modpath
	modAST <- lift $ parseFile filepath
	case modAST of
		Left err  -> throwError (CustomError $ show err)
		Right ast -> do
			modEnv <- evalProgram ast Map.empty
			return (modEnv `Map.union` env)
