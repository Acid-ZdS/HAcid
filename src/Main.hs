{-# LANGUAGE LambdaCase #-}

import Language.Acid

import Control.Monad.Trans (lift)
import Control.Monad.Except (throwError)

import qualified Data.Map as Map

import System.Environment (getArgs)


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


main :: IO ()
main = do
	(path:_) <- getArgs
	progAST  <- parseFile path
	case progAST of
		Left err  -> print err
		Right ast -> runEval (evalProgram ast defaultEnv) >> return ()
