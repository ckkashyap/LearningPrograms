import System.Environment
import Data.Char
import Data.List


reduceToSingleDigit n = if n < 10 then n else  reduceToSingleDigit (sumOfDigits n)

sumOfDigits n = foldr (+) 0 (map digitToInt (show n))

readNumber string =  foldr (+) 0 (map ((+1).(+(-65)).ord.toUpper) string)

process string = (readNumber string) 


goodwords = [
	"happiness",
	"successful",
	"prosperous",
	"victory",
	"triumphant",
	"achievement",
	"victorious",
	"blissful",
	"lucrative"

	]


wordMessageTuples = [
	("happiness", "2011 is clearly your year for ultra happy times."),
	("successful", "2011 will clearly be filled with your successes."),
	("prosperous", "2011 will clearly be very very prosperous for you."),
	("victory", "2011 will be filled with your victories."),
	("triumphant", "2011 will be the year of your triumph."),
	("achievement", "2011 will be the year filled with your achievements."),
	("victorious", "2011 will be filled with your victories."),
	("blissful", "2011 is clearly your year for ultra happy times."),
	("lucrative", "2011 will clearly be very very lucrative for you.")
	]

specialMessage w ((sw,sm):rest)
				| w == sw = sm
				| otherwise = specialMessage w rest
	

year = 2011

formula1 number  = reduceToSingleDigit (number * year)

numbers xs = sort $ foldr ff [] xs
	where
		(n,w) `ff` xs = n:xs

getTuple input (t@(n,w):xs) = if n==input then t  else getTuple input xs

tuples = foldr ff [] goodwords
	where
		i `ff` xs = (formula1 (readNumber i),i) :  xs


nameString1 name = (intersperse '+' name) ++ " => " ++ (concat $ intersperse "+" $ foldr ff [] name) ++ " you get "  ++ show (readNumber name)
	where
		i `ff` xs = show ((+(-64)) . ord . toUpper $ i ) : xs

nameString name = concat $ (intersperse " + " $ foldr ff [] name) ++ [" you get "] ++ [show (readNumber name)]
	where
		i `ff` xs = ([toUpper i] ++ "=" ++ show ((+(-64)) . ord . toUpper $ i )) : xs

message (n,w) word = "Hi " ++ word ++ ",\nI wish you and your family a very very happy new year. By the way, if you add up the letters of your name (using a=1, b=2 and so on ) " ++ nameString1 word ++ ". If you multiply that with 2011(the new year), you get " ++ show (year * (readNumber word)) ++ ". Reducing that to a single digit by repeated addition of the digits produces " ++ (show n) ++ ". Interestingly, when \"" ++ w ++  "\" is put through the same computation, it produces " ++ (show n)  ++ ". Coincidence? ... I don't think so!!! " ++ specialMessage w wordMessageTuples ++"\nRegards,\nKashyap"



main = do
	args <- getArgs
	let word =  args !! 0
	let input = formula1 (readNumber word)
	let tuple = getTuple input tuples
	putStrLn (message tuple word)
