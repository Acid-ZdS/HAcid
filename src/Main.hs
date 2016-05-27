import Language.Acid
import System.Environment (getArgs)


main :: IO ()
main = do
	(path:_) <- getArgs
	ast <- parseFile path
	print (forceRight ast)
