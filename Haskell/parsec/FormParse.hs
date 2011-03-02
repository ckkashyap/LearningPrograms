import Text.ParserCombinators.Parsec
import Numeric (readHex)

data Tree a = Nil | Node a (Tree a ) (Tree a) deriving (Show)

data Element = Number Int | Plus | Minus | Multiply | Divide deriving (Show)

type AST = Tree Element


parseAST :: CharParser () AST
parseAST = parseAdd

parseAdd :: CharParser () AST
parseAdd = do
	e1 <- parseMul
	(rest e1) <|> return e1
	where
		rest e1 = do
			string "+"
			e2 <- parseAdd
			return (Node Plus e1 e2)

parseMul :: CharParser () AST
parseMul = do
	e1 <- parseBase
	(rest e1) <|> return e1
	where
		rest e1 = do
			string "*"
			e2 <- parseMul
			return (Node Multiply e1 e2)

parseBase :: CharParser () AST
parseBase = do
	withParens <|> numParser
	where
		withParens = do
				string "("
				e <- parseAST
				string ")"
				return e


parsePlus :: CharParser () AST
parsePlus = do
	return (Node (Number 1) Nil Nil)

numParser :: CharParser () (Tree Element)
numParser = do
	c <- many1 (oneOf "1234567890")
	return (Node (Number (read c:: Int)) Nil Nil)
