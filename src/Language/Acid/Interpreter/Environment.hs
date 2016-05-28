{-# LANGUAGE
	MultiParamTypeClasses
  , TypeSynonymInstances
  , FlexibleInstances
  #-}

module Language.Acid.Interpreter.Environment where

import Language.Acid.Types

import qualified Data.Map as Map

type Scope a = Map.Map Name a


class Environment e a where
	declare :: Name -> a -> e -> e
	fetch   :: Name -> e -> Maybe a

instance Environment (Scope a) a where
	declare = Map.insert
	fetch = Map.lookup
