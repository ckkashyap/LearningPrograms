import Text.ParserCombinators.Parsec

data XML =  Node String [XML]
          | Body String deriving Show


gettext = fmap Body $ many1 $ letter <|> digit

xml :: Parser XML
xml = do
    tag <- openTag
    innerXML <- many innerXML
    endTag tag
    return (Node tag innerXML) 

innerXML = xml <|> gettext

openTag :: Parser String
openTag = do
        try $ char '<' >> notFollowedBy (char '/')
        tag <- many (noneOf ">")
        char '>'
        return tag

endTag :: String -> Parser ()
endTag str = do
       try $ string "</" >> string str >> char '>' >> return ()


h1 = parse xml "" "<a>A</a>"
h2 = parse xml "" "<a><b>A</b></a>"
h3 = parse xml "" "<a><b><c></c></b></a>"
h4 = parse xml "" "<a><b></b><c></c>HELLO</a>"
h5 = parse xml "" "<a><b></b><c></c>HELLO</a>abcd"