import Control.Monad (ap, liftM2)
import Text.ParserCombinators.Parsec
import Numeric (readHex)

queryParser :: CharParser () [(String, Maybe String)]
queryParser = pairParser `sepBy` char '&'

pairParser :: CharParser () (String, Maybe String)
pairParser = do
  name <- many1 charParser
  value <- optionMaybe (char '=' >> many charParser)
  return (name, value)

charParser :: CharParser () Char
charParser = oneOf urlBaseChars
     <|> (char '+' >> return ' ')
     <|> hexParser

urlBaseChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_.!*'(),"

hexParser :: CharParser () Char
hexParser = do
  char '%'
  a <- hexDigit
  b <- hexDigit
  let ((d, _):_) = readHex [a,b]
  return . toEnum $ d





data Tree a = Nil | Node a (Tree a ) (Tree a) deriving (Show)

data Element = Number Int | Plus | Minus | Multiply | Divide deriving (Show)



expressionParser :: CharParser () (Tree Element)
expressionParser = 
	do
		n1 <- numParser
		(Node o _ _) <- operatorParser
		n2 <- numParser
		return (Node o n1 n2)

numParser :: CharParser () (Tree Element)
numParser = do
	c <- many1 (oneOf "1234567890")
	return (Node (Number (read c:: Int)) Nil Nil)


operatorParser :: CharParser () (Tree Element)
operatorParser =
		do 
			char '+'
			return (Node Plus Nil Nil)
	<|> do 
		char '-'
		return (Node Minus Nil Nil)
	<|> do 
		char '*'
		return (Node Multiply Nil Nil)
	<|> do 
		char '/'
		return (Node Divide Nil Nil)

	




