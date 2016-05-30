{-# LANGUAGE LambdaCase #-}

import Language.Acid

import System.Console.ArgParser.SubParser
import System.Console.ArgParser


data Action
	= RunFile FilePath
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
		Right ast -> runEval (evalProgram ast) >>=
			\case
				Right _  -> return ()
				Left err -> print err

runAction (RunExpr expr) = do
	case parseExpr expr of
		Left  err -> print err
		Right ast -> runEval (evalExpr ast defaultEnv) >>=
				\case
					Right _  -> return ()
					Left err -> print err

main :: IO ()
main = runApp commandLineParser runAction
