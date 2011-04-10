module Resume.Main where

import System.Environment

import Resume.Summary
import Resume.Highlights
import Resume.Beautify
import Resume.Type
import Resume.ProfessionalHistory


generateF t HTML = do
	putStrLn "<html><body><font face=\"courier\">"
	putStrLn $ title HTML "C K Kashyap" "ckkashyap@gmail.com" "BITS Pilani 1996 - 2000"
	putStrLn $ subHeading HTML "Summary"
	putStrLn (summary t HTML)
	putStrLn $ subHeading HTML "Highlights"
	putStrLn (highlights t HTML)
	putStrLn $ subHeading HTML "Work experience details"
	putStrLn $ printWork HTML history
	putStrLn "</body></html>"

generateF t TXT = do
	putStrLn $ title TXT "C K Kashyap" "ckkashyap@gmail.com" "BITS Pilani 1996 - 2000"
	putStrLn $ subHeading TXT "Summary"
	putStrLn (summary t TXT)
	putStrLn $ subHeading TXT "Highlights"
	putStrLn (highlights t TXT)
	putStrLn $ subHeading TXT "Work experience details"
	putStrLn $ printWork TXT history



generate t = do
	generateF t TXT



main=do
	args <- getArgs
	case args of
		("dev":_)	-> generate Developer
		("mgr":_)	-> generate Manager
		_		-> putStrLn "Incorrect usage"
