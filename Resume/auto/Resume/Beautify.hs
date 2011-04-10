module Resume.Beautify where

import Resume.Type
import Resume.ProfessionalHistory

import Data.List
import Text.Regex


makeParagraphs :: Format -> String -> String
makeParagraphs HTML s = wrapTag "p" s
makeParagraphs TXT s = '\n':'\t':s ++ "\n"


wrapTag tag str = openTag ++ str ++ closeTag
	where
		openTag = '<':tag ++ ">"
		closeTag = '<':'/':tag ++ ">"
		
italic = wrapTag "i"
bold = wrapTag "b"

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

insertAnchorLinks str = subRegex linkReg str "<a href=\"\\1\">\\1</a>"
	where
		linkReg = mkRegex "(https?://[a-zA-Z0-9./]+)"



printWork  :: Format -> WorkHistory -> String
printWork _ [] = [] 
printWork HTML (w:ws) =  (printWork' HTML w) ++ "<br><br>" ++ (printWork HTML ws)
printWork TXT (w:ws)  = (printWork' TXT w) ++ "\n\n" ++ (printWork TXT ws)


printWork' HTML (WorkAtCompany company designation start end description) = concat $ intersperse "<br>" [
		field "Company" (bold company),
		field "Designation" designation,
		field "Duration" (start ++ " - " ++ end),
		field "Description" description
	]
	where
		field name value = (italic name) ++ ": " ++ value 

printWork' TXT (WorkAtCompany company designation start end description) = concat $ intersperse "\n" [
		field "Company" company,
		field "Designation" designation,
		field "Duration" (start ++ " - " ++ end),
		field "Description" description
	]
	where
		field name value = name ++ ": " ++ value 
