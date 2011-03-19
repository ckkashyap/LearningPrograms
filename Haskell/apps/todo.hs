import Data.Time
import System.IO
import Control.Monad.State

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

prompt :: String -> MyState ()
prompt str = liftIO $ do
	putStr str
	hFlush stdout
	return ()

addTask :: MyState ()
addTask =  do
	liftIO $ putStrLn "Adding a Task"
	prompt "Enter the description: "
	desc <- liftIO $ getLine
	t <- get
	ct <- liftIO getCurrentTime
	let newTask = Task {description = desc, dueDate = ct, updates=[]}
	put (newTask:t)
	return ()


listTask :: MyState ()
listTask = do
	liftIO $ putStrLn "Will list the tasks"
	t <- get
	liftIO $ putStrLn (show t)
	return ()

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



menu = [
	(addTask, "Add a task"),
	(listTask, "List tasks"),
	(load,"Load"),
	(save,"Save")
	]

	
doMenu :: MyState ()
doMenu = do
	showMenu' 1 menu
	option <- getOption
	if (valid option) then
		do
			let (action,_) = menu!!(option-1)
			action
			doMenu
		else
			do
				liftIO $ putStrLn "Invalid Option"
				doMenu
	where
		showMenu' n ((_,s):es) = do
			liftIO $ putStrLn $ (show n) ++ ". " ++ s 
			showMenu' (n+1) es
		showMenu' _ [] = return ()
		getOption = do
			prompt "Enter option: "
			x <- liftIO $ getLine
			let o = read x :: Int
			return o
		valid o = (o > 0) && (o <= (length menu))

	


main :: IO ()
main = do
	(s, t) <- runStateT doMenu []
	putStrLn (show t)
	return ()
--	
