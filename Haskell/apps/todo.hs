{-# LANGUAGE NamedFieldPuns #-}

import Data.Time
import System.IO
import Control.Monad.State

import Util

import qualified Data.Map as Map
import qualified Command as Command

type KeyValueMap = Map.Map String String


data Task = Task {
	taskId :: Int,
	description :: String,
	dueDate :: Day,
	updates :: [Update],
	tags :: [String]
} deriving (Show,Read)

data Update = Update {
	message :: String,
	date :: Day
}deriving (Show,Read)

type Tasks = [Task]


data TaskState = TaskState {
	tasks :: [Task],
	maxId :: Int
} deriving (Show,Read)


type MyState a = StateT TaskState IO a


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
	let tags = split (lookupMap "t" params) ','
	let day = parseDay (lookupMap "o" params) today
	liftIO $ putStrLn $ desc ++ " is due on " ++ (show day)
	TaskState {tasks,maxId } <- get
	let newTask = Task {taskId = 1, description = desc, dueDate = day, updates=[], tags=tags}
	put (TaskState {tasks=newTask:tasks,maxId=maxId+1})
	return ()


--listTask :: MyState ()
--listTask = do
--	tasks <- get
--	liftIO $ listTask' 1 tasks
--	return ()
--	where
--		listTask' _ [] = return ()
--		listTask' n ((Task {description,dueDate,updates}):ts) = do
--			putStrLn $ (show n) ++ ". "++ description 
--			listTask' (n+1) ts
--
backupFile = "backup.txt"
--
--load :: MyState ()
--load = do
--	handle <- liftIO $ openFile backupFile ReadMode
--	contents <- liftIO $ hGetContents handle
--	let tasks = read contents :: Tasks
--	liftIO $ putStrLn ("Loaded " ++ (show.length $ tasks) ++ " tasks!" )
--	liftIO $ hClose handle
--	put tasks
--	return ()
--
save :: MyState ()
save = do
	taskState <- get
	liftIO $ writeFile backupFile (show taskState)
	return ()


doTasks :: MyState ()
doTasks = do
	liftIO $ putStr "Enter command: " >> hFlush stdout
	line <- liftIO getLine
	let (cmd,params) = Command.getCommand line
	case cmd of
		"q"	-> return ()
		"a"	-> addTask params >> doTasks
		--"l"	-> listTask >> doTasks
		"s"	-> save >> doTasks
		--"ld"	-> load >> doTasks
		_	-> doTasks



main :: IO ()
main = do
	--(s, t) <- runStateT doMenu []
	(s, t) <- runStateT doTasks (TaskState {tasks=[],maxId=0})
	return ()


--main = do
--	line <- getLine
--	(command,_) <- Command.getCommand line
--	main
