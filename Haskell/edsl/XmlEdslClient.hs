import XmlEdsl
import Data.Tree


nameAge name age = do
	addContainer (getTag "Personal details")
	addLeaf (getTag name)
	addLeaf (getTag (show age))
	closeTag


--doXML :: XMLContext ()
doXML = do
	addContainer (getTag "Info")
	"kashyap" `nameAge` 33
	nameAge "abcd" 33
	addContainer (getTag "1")
	addContainer (getTag "2")
	addContainer (getTag "3")
	addContainer (getTag "4")
	closeTag
	closeTag
	closeTag
	closeTag
	addLeaf (getTag "should be under XML")
	return ()
	


main = do
	xml <- process doXML
	putStrLn (drawTree (fmap show xml))

	return ()
