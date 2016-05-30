module Language.Acid.Interpreter (
	runEval
  , module Language.Acid.Interpreter.Program
  , module Language.Acid.Interpreter.Expr
  , module Language.Acid.Interpreter.Environment
  , module Language.Acid.Interpreter.DefaultEnv
  , module Language.Acid.Interpreter.Types
) where

import Language.Acid.Interpreter.Program
import Language.Acid.Interpreter.Expr
import Language.Acid.Interpreter.Environment
import Language.Acid.Interpreter.DefaultEnv
import Language.Acid.Interpreter.Types

import Control.Monad.Trans.Except (ExceptT, runExceptT)

runEval :: ExceptT EvalError IO a -> IO (Either EvalError a)
runEval = runExceptT
