module Command (getCommand) where

import Text.ParserCombinators.Parsec
import qualified Data.Map as Map

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

nameValuePair :: CharParser () (Key,Value)
nameValuePair = do
	spaces
	name <-identifier
	spaces
	char '='
	spaces
	char '"'
	value <- many (noneOf "\"") -- TODO - we cannot get " into the string for now
	char '"'
	return (name,value)
	
--commandParser :: CharParser () Command
commandParser = do
	cmd <- identifier
	nv <- many (nameValuePair)
	return (cmd, Map.fromList nv)

--getCommand :: String -> 
getCommand str = let result = parse commandParser "" str
	in
		case result of
			(Right command) -> command
			_		-> ("BadCommand",Map.empty)
	

