module Makefile where


type Target = String
type Dependencies = [Rule]
type Action = IO ()

data Rule = Rule { 
	target :: Target,
	dependsOn :: Dependencies,
	action :: Action
} 

instance Show Rule where
	show (Rule t ds a) = (show t) ++ " depend on " ++ dependencies 
		where dependencies = show (map target ds)

build :: Rule -> IO ()
build r@(Rule t d a) = do 
			putStrLn (show r)
			a



