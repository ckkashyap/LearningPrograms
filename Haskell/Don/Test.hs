import Makefile

r1 = Rule {
	target = "file1",
	dependsOn = [r2,r3],
	action = do 
			putStrLn ("Going to build " ++ (show (target r1)) ++ " by compiling " )
			putStrLn "World"
} 

r2 = Rule {
	target = "file2",
	dependsOn = [],
	action = do 
			putStrLn ("Going to build " ++ (show (target r2)) ++ " by compiling " )
			putStrLn "World"
} 

r3 = Rule {
	target = "file3",
	dependsOn = [],
	action = do 
			putStrLn ("Going to build " ++ (show (target r3)) ++ " by compiling " )
			putStrLn "World"
} 

main=do
	build r1
	build r2
