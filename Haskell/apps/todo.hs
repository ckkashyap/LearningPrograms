{-# LANGUAGE NamedFieldPuns #-}

import Data.Time
import System.IO
import Control.Monad.State

import qualified Data.Map as Map
import qualified Command as Command

type KeyValueMap = Map.Map String String


data Task = Task {
	description :: String,
	dueDate :: Day,
	updates :: [Update]
} deriving (Show,Read)

data Update = Update {
	message :: String,
	date :: Day
}deriving (Show,Read)

type Tasks = [Task]

type MyState a = StateT Tasks IO a


utcTimeToDay :: UTCTime -> Day
utcTimeToDay u = read (take 10 $ show u) :: Day

parseInt :: String -> Int
parseInt str = let parse = reads str :: [(Int,String)] in
	case parse of
		[(i,_)] -> i
		_ -> 0

parseDay :: String -> Day -> Day
parseDay str today = let parse = reads str :: [(Day,String)] in
	case parse of
		[(d,_)] -> d
		_       -> today

lookupMap key map = case (Map.lookup key map) of
	Just v	-> v
	_	-> ""

addTask :: (Map.Map String String) -> MyState ()
addTask params =  do
	ct <- liftIO $ getCurrentTime
	let today = utcTimeToDay ct
	let desc = lookupMap "s" params
	let day = parseDay (lookupMap "o" params) today
	liftIO $ putStrLn $ desc ++ " is due on " ++ (show day)
	task <- get
	let newTask = Task {description = desc, dueDate = day, updates=[]}
	put (newTask:task)
	return ()


listTask :: MyState ()
listTask = do
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
		"a"	-> addTask params >> doTasks
		"l"	-> listTask >> doTasks
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