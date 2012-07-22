import Text.ParserCombinators.Parsec

data XML =  Node String [(String,String)] [XML]
          | Body String deriving Show


gettext = fmap Body $ many1 $ noneOf "<>"

xml :: Parser XML
xml = try withoutExplictCloseTag <|>  withExplicitCloseTag



withExplicitCloseTag :: Parser XML
withExplicitCloseTag = do
	        (tag,a) <- openTag
	        innerXML <- many innerXML
	        closeTag tag
	        return (Node tag a innerXML)

  


openTag :: Parser (String, [(String,String)])
openTag = do
        try $ char '<' >> notFollowedBy (char '/')
        tag <- many (letter <|> digit)
        spaces
        a <- try (many keyValue)
        char '>'
        return (tag, a)

closeTag :: String -> Parser ()
closeTag str = do
       try $ string "</" >> string str >> char '>' >> return ()



withoutExplictCloseTag :: Parser XML
withoutExplictCloseTag = do
	        try $ char '<' >> notFollowedBy (char '/')
	        tag <- many (letter <|> digit)
                spaces
	        a <- try (many keyValue)
                spaces
                string "/>"
                return (Node tag a [])
                   


keyValue :: Parser (String, String)
keyValue = do
         key <- many1 (letter <|> digit)
         spaces
         char '='
         spaces
         value <- quotedString
         spaces
         return (key, value)

quotedString = 
    do c <- (try (char '"')) <|> char '\''
       content <- many (quotedChar c)
       char c <?> "quote at end of cell"
       return content

quotedChar c =
           try (string ['\\', c] >> return c)        
           <|> noneOf [c]

innerXML = xml <|> gettext






h1 = parse xml "" "<a>A</a>  "
h2 = parse xml "" "<a><b>A</b></a>"
h3 = parse xml "" "<a><b><c></c></b></a>"
h4 = parse xml "" "<a><b></b><c></c>HELLO</a>"
h5 = parse xml "" "<a><b></b><c></c>HELLO</a>abcd"
h6 = parse xml "" "<a k1='v1' k2='v2'><b></b><c></c>HELLO</a>abcd"
h7 = parse xml "" "<a k1='v1' k2='v2'   ><b>aa</b><c>aaa</c>HELLO</a>abcd"
h8 = parse xml "" "<a k1='v1' k2='v2'>a's body <b></b> <c/></a>"



qs = "\"abcd\\\"hello world\""

k1 = parse keyValue "" "hello = \"world\""
k2 = parse keyValue "" "hello = \"world\\\"\""

k3 = parse    quotedString ""


testParser :: Parser a -> Parser (a, String, String)
testParser p = do
           before <- getInput
           o <- p
           after <- getInput
           return (o, before, after)


main = do
     str <- readFile "test.xml"
     let n = parse xml "" str
     putStrLn (show n)
     return ()