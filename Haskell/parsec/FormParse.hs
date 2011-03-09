import System.Environment
import Text.ParserCombinators.Parsec
import Numeric (readHex)

data Tree a = Nil | Node a (Tree a ) (Tree a) deriving (Show)

data Element = Number Float | Plus | Minus | Multiply | Divide deriving (Show)

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


numParser :: CharParser () (Tree Element)
numParser = do
	many (char ' ')
	f <- startWithDot <|> startWithDigit
	many (char ' ')
	return (Node (Number (read f:: Float)) Nil Nil)
	where
		startWithDigit = do
			do
				try $ do
					s1 <- many digit
					dot
					s2 <- digits
					return (s1 ++ ['.'] ++ s2)
				<|> do
					s1 <- many digit
					return (s1)
			<|> startWithDot
		startWithDot = do
			dot
			s1 <- digits
			return ("0."++s1)
		digits = many1 digit
		dot = char '.'

main=do
	args <- getArgs
	case args of
		(str:_) -> let result = parse parseAST "" str in
			putStrLn (show result)
		_	-> putStrLn "Insufficient args"
