module Command (getCommand) where

import qualified Data.Map as Map

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as TP
import Text.ParserCombinators.Parsec.Language( javaStyle )


stringLiteral   = TP.stringLiteral (TP.makeTokenParser javaStyle)

type Key	= String
type Value	= String

data Command = 
	  Command {command :: String, parameters :: Map.Map String String}
	| BadCommand
	deriving (Show)

identifier :: CharParser () String
identifier = do
	c <- letter
	cs <- many (alphaNum <|> char '_')
	return (c:cs)

nameValuePairQuoted :: CharParser () (Key,Value)
nameValuePairQuoted = try $ do
	spaces
	name <-identifier
	spaces
	char '='
	spaces
	value <- stringLiteral
	return (name,value)


nameValuePair :: CharParser () (Key,Value)
nameValuePair = try $ do
	spaces
	name <-identifier
	spaces
	char '='
	spaces
	value <- many (noneOf " ") 
	return (name,value)
	
--commandParser :: CharParser () Command
commandParser = do
	cmd <- identifier
	nv <- many (nameValuePairQuoted <|> nameValuePair)
	return (cmd, Map.fromList nv)

--getCommand :: String -> 
getCommand str = let result = parse commandParser "" str
	in
		case result of
			(Right command) -> command
			_		-> ("BadCommand",Map.empty)
	

