{-# LANGUAGE NamedFieldPuns #-}

module Command (Command (..),getCommand) where

import Data.Time
import System.IO
import Control.Monad.State
import Text.ParserCombinators.Parsec


data Command = 
	  New {description :: String, dueDate :: UTCTime}
	| ListAll
	| ListParticular {taskId :: Int}
	| UpdateParticular {taskId :: Int,description :: String}
	| BadCommand
	deriving (Show)


tryNew :: CharParser () Command
tryNew = try $ do
	string "new"
	char ' ' >> many (char ' ') -- on or more spaces
	year <- nDigits 4
	char '-'
	month <- nDigits 2
	char '-'
	day <- nDigits 2
	char ' ' >> many (char ' ') -- on or more spaces
	desc <- many $ noneOf "."
	char '.'
	let t = read (year ++ "-" ++ month ++ "-" ++ day ++ " 00:00:00.000000 UTC") :: UTCTime -- hack TODO
	return New {description = desc,dueDate=t}
	where
		nDigits 0 = return ""
		nDigits n = do
			d <- digit
			ds <- nDigits (n-1)
			return (d:ds)

tryListAll :: CharParser () Command
tryListAll = try $ do
	string "list"
	many (char ' ')
	string "all"
	return ListAll

tryListParticular = try $ do
	string "list"
	many (char ' ')
	n <- many1 digit
	let tid = read n :: Int
	return ListParticular {taskId = tid}


commandParser :: CharParser () Command
commandParser = 
	    tryNew
	<|> tryListParticular
	<|> tryListAll


getCommand :: String -> Command
getCommand str = let result = parse commandParser "" str
	in
		case result of
			(Right command) -> command
			_		-> BadCommand
	

