import System.Environment
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@_~"

--readExpr :: String -> String
--readExpr input = case parse (spaces >> symbol) "lisp" input of
--	Left err -> "No match: " ++ show err
--	Right val -> "Found value"


main :: IO ()
main = do
	args <- getArgs
	putStrLn (readExpr (args !! 0))


spaces :: Parser ()
spaces = skipMany1 space
		

data LispVal = Atom String
	| List [LispVal]
	| DottedList [LispVal]
	| Number Integer
	| String String
	| Bool Bool
	deriving (Show)


parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x


parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of 
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit




parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right v -> "Found value " ++ show v

