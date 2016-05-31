{-# LANGUAGE LambdaCase #-}

module Language.Acid.Interpreter.DefaultEnv (defaultEnv) where

import Language.Acid.Interpreter.Environment
import Language.Acid.Interpreter.Types
import Language.Acid.Types

import Control.Monad.Trans (lift)
import Control.Monad.Except (throwError)

import qualified Data.Map as Map


defaultEnv :: Scope Value
defaultEnv = Map.fromList [
		("+",     addF)
	  , ("-",     subF)
	  , ("*",     mulF)
	  , ("/",     divF)
	  , ("print", printF) ]
	where
		-- Variadic [default value] [minimum arity] [reduce function]
		addF = Variadic (IntV 0) 2 $ \ a b -> case (a, b) of
				(IntV x, IntV y) -> return $ IntV (x + y)
				(IntV x, FltV y) -> return $ FltV (fromInteger x + y)
				(FltV x, IntV y) -> return $ FltV (x + fromInteger y)
				(FltV x, FltV y) -> return $ FltV (x + y)
				_                -> throwError (CustomError "Expected integer of float")

		subF = Variadic (IntV 0) 2 $ \ a b -> case (a, b) of
				(IntV x, IntV y) -> return $ IntV (x - y)
				(IntV x, FltV y) -> return $ FltV (fromInteger x - y)
				(FltV x, IntV y) -> return $ FltV (x - fromInteger y)
				(FltV x, FltV y) -> return $ FltV (x - y)
				_                -> throwError (CustomError "Expected integer of float")

		mulF = Variadic (IntV 1) 2 $ \ a b -> case (a, b) of
				(IntV x, IntV y) -> return $ IntV (x * y)
				(IntV x, FltV y) -> return $ FltV (fromInteger x * y)
				(FltV x, IntV y) -> return $ FltV (x * fromInteger y)
				(FltV x, FltV y) -> return $ FltV (x * y)
				_                -> throwError (CustomError "Expected integer of float")

		divF = Variadic (FltV 1) 2 $ \ a b -> case (a, b) of
				(IntV x, IntV y) -> return $ FltV (fromInteger x / fromInteger y)
				(IntV x, FltV y) -> return $ FltV (fromInteger x / y)
				(FltV x, IntV y) -> return $ FltV (x / fromInteger y)
				(FltV x, FltV y) -> return $ FltV (x / y)
				_                -> throwError (CustomError "Expected integer of float")

		printF = HLam $ \ arg -> lift (print arg) >> return UnitV
