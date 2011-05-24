module Resume.Main where

import System.Environment

import Resume.Summary
import Resume.Highlights
import Resume.Beautify
import Resume.Type
import Resume.ProfessionalHistory


generateF t HTML = "<html><body><font face=\"arial\">"
	++ title HTML "C K Kashyap" "ckkashyap@gmail.com" "BITS Pilani 1996 - 2000" "9448466112"
	++ subHeading HTML "Summary"
	++ (summary t HTML)
	++ subHeading HTML "Highlights"
	++ (highlights t HTML)
	++ subHeading HTML "Work experience details"
	++ printWork HTML history
	++ "</body></html>"

generateF t TXT = title TXT "C K Kashyap" "ckkashyap@gmail.com" "BITS Pilani 1996 - 2000" "9448466112"
	++ "\n" ++  subHeading TXT "\nSummary"
	++ (summary t TXT)
	++ "\n" ++  subHeading TXT "Highlights"
	++ "\n" ++  (highlights t TXT)
	++ "\n" ++  subHeading TXT "Work experience details"
	++ "\n" ++  printWork TXT history



generate t = do
	writeFile "out.txt" (generateF t TXT)
	writeFile "out.html" (generateF t HTML)



main=do
	args <- getArgs
	case args of
		("dev":_)	-> generate Developer
		("mgr":_)	-> generate Manager
		_		-> putStrLn "Incorrect usage"
