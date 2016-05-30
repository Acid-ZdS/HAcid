module Language.Acid.Interpreter.Program where

import Language.Acid.Parser
import Language.Acid.Interpreter.Expr
import Language.Acid.Interpreter.Environment
import Language.Acid.Interpreter.DefaultEnv
import Language.Acid.Interpreter.Types

import Control.Applicative
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Except (throwError)

import Data.List (intercalate)
import Data.Map ((!))
import qualified Data.Map as Map

import System.FilePath (dropFileName)
import System.Directory (getCurrentDirectory)
import Debug.Trace


evalProgram :: Program -> ExceptT EvalError IO (Scope Value)
evalProgram (Program maybePath instrs) = do
	env <- case maybePath of
		Just path -> lift . return $ declare "__file__" (StrV path) defaultEnv
		Nothing   -> lift $ declare "__file__" . StrV <$> getCurrentDirectory <*> pure defaultEnv

	evalStmtList instrs env

evalStmtList :: [Statement] -> Scope Value -> ExceptT EvalError IO (Scope Value)
evalStmtList [] env = return env
evalStmtList (instr:instrs) env = evalStmtList instrs =<< evalStmt instr env

evalStmt :: Statement -> Scope Value -> ExceptT EvalError IO (Scope Value)
evalStmt (Define name aval) env = do
	val <- evalExpr aval env
	return (declare name val env)
evalStmt (TLExpr expr) env = evalExpr expr env >> return env
evalStmt (Import modpath) env = do
		modAST <- lift $ parseFile abspath
		case modAST of
			Left  err -> throwError (ImportError abspath err)
			Right ast -> do
				modEnv <- evalProgram ast
				return (modEnv `Map.union` env)
	where
		relpath = intercalate "/" modpath ++ ".acid"
		(StrV pwd) = env ! "__file__"
		abspath = dropFileName pwd ++ relpath
