{-# LANGUAGE NamedFieldPuns #-}

import Data.Time
import System.IO
import Control.Monad.State

import qualified Data.Map as Map
import qualified Command as Command

type KeyValueMap = Map.Map String String

data Command = 
	  New {newDescription :: String, newDueDate :: UTCTime}
	| ListAll
	| ListParticular {id :: Int}
	| UpdateParticular {id :: Int}
	deriving (Show)

data Task = Task {
	description :: String,
	dueDate :: UTCTime,
	updates :: [Update]
} deriving (Show,Read)

data Update = Update {
	message :: String,
	date :: Day
}deriving (Show,Read)


type Tasks = [Task]

type MyState a = StateT Tasks IO a


decoratedPrint :: String -> IO ()
decoratedPrint str = do
	putStrLn ""
	putStrLn fullLine
	putStrLn $ prefix ++ message ++ postfix
	putStrLn fullLine

	where
		message = take 20 str
		fullLine = take fullLineLength (repeat '*')
		prefix = decorLengthStars ++ " "
		postfix = " " ++ decorLengthStars
		decorLengthStars = take decorLength (repeat '*')
		decorLength = 5
		fullLineLength = messageLength + 2 + (decorLength*2)
		messageLength = length message

prompt :: String -> MyState ()
prompt str = liftIO $ do
	putStr str
	hFlush stdout
	return ()

addTask :: (Map.Map String String) -> MyState ()
addTask p =  do
	liftIO $ putStrLn "Adding a Task"
	let (Just desc) = Map.lookup "desc" p
	let (Just dt) = Map.lookup "due" p
	let date = dt ++ " 00:00:00.000000 IST"
	let ((utcTime,_):_) = reads date :: [(UTCTime,String)]
	liftIO $ putStrLn (show utcTime)
	task <- get
	let newTask = Task {description = desc, dueDate = utcTime, updates=[]}
	put (newTask:task)
	return ()


listTask :: MyState ()
listTask = do
	liftIO $ decoratedPrint "Task list"
	tasks <- get
	liftIO $ listTask' 1 tasks
	return ()
	where
		listTask' _ [] = return ()
		listTask' n ((Task {description,dueDate,updates}):ts) = do
			putStrLn $ (show n) ++ ". "++ description 
			listTask' (n+1) ts

backupFile = "backup.txt"

load :: MyState ()
load = do
	handle <- liftIO $ openFile backupFile ReadMode
	contents <- liftIO $ hGetContents handle
	let tasks = read contents :: Tasks
	liftIO $ putStrLn ("Loaded " ++ (show.length $ tasks) ++ " tasks!" )
	liftIO $ hClose handle
	put tasks
	return ()

save :: MyState ()
save = do
	tasks <- get
	liftIO $ writeFile backupFile (show tasks)
	return ()


doTasks :: MyState ()
doTasks = do
	liftIO $ putStrLn "Enter command:"
	line <- liftIO getLine
	let (cmd,params) = Command.getCommand line
	case cmd of
		"quit"	-> return ()
		"add"	-> addTask params >> doTasks
		"list"	-> listTask >> doTasks
		_	-> doTasks



main :: IO ()
main = do
	--(s, t) <- runStateT doMenu []
	(s, t) <- runStateT doTasks []
	putStrLn (show t)
	return ()


--main = do
--	line <- getLine
--	(command,_) <- Command.getCommand line
--	main
