{-# LANGUAGE LambdaCase #-}

import Language.Acid

import Control.Monad.Trans (lift)
import Control.Monad.Except (throwError)

import qualified Data.Map as Map

import System.Console.ArgParser.SubParser
import System.Console.ArgParser


defaultEnv :: Scope Value
defaultEnv = Map.fromList [
		("+",     addF)
	  , ("-",     subF)
	  , ("*",     mulF)
	  , ("/",     divF)
	  , ("print", printF) ]
	where
		addF = HLam $ \case
			IntV x -> return $ HLam $ \case
				IntV y -> return $ IntV (x + y)
				FltV y -> return $ FltV (fromInteger x + y)
				_      -> throwError (CustomError "Expected integer of float")
			FltV x -> return $ HLam $ \case
				IntV y -> return $ FltV (x + fromInteger y)
				FltV y -> return $ FltV (x + y)
				_      -> throwError (CustomError "Expected integer of float")
			_      -> throwError (CustomError "Expected integer of float")

		subF = HLam $ \case
			IntV x -> return $ HLam $ \case
				IntV y -> return $ IntV (x - y)
				FltV y -> return $ FltV (fromInteger x - y)
				_      -> throwError (CustomError "Expected integer of float")
			FltV x -> return $ HLam $ \case
				IntV y -> return $ FltV (x - fromInteger y)
				FltV y -> return $ FltV (x - y)
				_      -> throwError (CustomError "Expected integer of float")
			_      -> throwError (CustomError "Expected integer of float")

		mulF = HLam $ \case
			IntV x -> return $ HLam $ \case
				IntV y -> return $ IntV (x * y)
				FltV y -> return $ FltV (fromInteger x * y)
				_      -> throwError (CustomError "Expected integer of float")
			FltV x -> return $ HLam $ \case
				IntV y -> return $ FltV (x * fromInteger y)
				FltV y -> return $ FltV (x * y)
				_      -> throwError (CustomError "Expected integer of float")
			_      -> throwError (CustomError "Expected integer of float")

		divF = HLam $ \case
			IntV x -> return $ HLam $ \case
				IntV y -> return $ FltV (fromInteger x / fromInteger y)
				FltV y -> return $ FltV (fromInteger x / y)
				_      -> throwError (CustomError "Expected integer of float")
			FltV x -> return $ HLam $ \case
				IntV y -> return $ FltV (x / fromInteger y)
				FltV y -> return $ FltV (x / y)
				_      -> throwError (CustomError "Expected integer of float")
			_      -> throwError (CustomError "Expected integer of float")

		printF = HLam $ \ arg -> lift (print arg) >> return UnitV


data Action
	= RunFile String
	| RunExpr String

commandLineParser :: CmdLnInterface Action
commandLineParser = mkSubParserWithName "ZdS Acid Interpreter"
  [ ("exec", mkDefaultApp
      	(RunFile
			`parsedBy` reqPos "path" `Descr` "The path of the file to execute")
        "exec" `setAppDescr` "Interpret the given file")
  , ("expr", mkDefaultApp
      	(RunExpr
	  		`parsedBy` reqPos "expr" `Descr` "The expression to execute")
        "expr" `setAppDescr` "Run a given expression")
  ]

runAction :: Action -> IO ()
runAction (RunFile path) = do
	progAST <- parseFile path
	case progAST of
		Left  err -> print err
		Right ast -> runEval (evalProgram ast defaultEnv) >> return ()

runAction (RunExpr expr) = do
	case parseExpr expr of
		Left  err -> print err
		Right ast -> do
			res <- runEval (evalExpr ast defaultEnv)
			case res of
				Right val -> print val
				Left  err -> print err

main :: IO ()
main = runApp commandLineParser runAction
