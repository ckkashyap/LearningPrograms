module Resume.Beautify where

import Resume.Type
import Resume.ProfessionalHistory

import Data.List


makeParagraphs :: Format -> String -> String
makeParagraphs HTML s = wrapTag "p" s
makeParagraphs TXT s = '\n':'\t':s ++ "\n"


wrapTag tag str = openTag ++ str ++ closeTag
	where
		openTag = '<':tag ++ ">"
		closeTag = '<':'/':tag ++ ">"
		
italic str = wrapTag "i" str

listify :: Format -> [String] -> [String]
listify HTML list = ["<ul>"] ++ (map (wrapTag "li") list) ++ ["</ul>"]
listify TXT list = map ((++"\n").("\t- "++)) list


subHeading :: Format -> String -> String
subHeading HTML str = wrapTag "h3" str
subHeading TXT str = str ++ "\n"

title HTML name email education = wrapTag "center" $ titleText
	where
		titleText = concat $ intersperse "<br>" [
			wrapTag "b" name,
			italic $ "<a href=\"mailto:" ++ email ++ "\">" ++ email ++ "</a>",
			italic education
			]

title TXT name email education = titleText
	where
		titleText = concat $ intersperse "\n" [
			name,
			email,
			education
			]



breakLine HTML n []  = []
breakLine HTML n str = (take n str) ++ "<br>" ++ breakLine HTML n (drop n str)


printWork  :: Format -> WorkHistory -> String
printWork HTML [] = [] 
printWork HTML ((WorkAtCompany company designation start end description):ws) =  company ++ "<br>" ++ (printWork HTML ws)
