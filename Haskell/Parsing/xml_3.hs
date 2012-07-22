import Text.ParserCombinators.Parsec

type Key = String
type Value = String
type Tag = String
type Attribute = (Key, Value)

data XML_AST =  
  Node Tag [Attribute] [XML_AST]
  | Body String
  deriving Show


getBody = fmap Body $ many1 $ noneOf "<>"

parseXML :: Parser XML_AST
parseXML =
  do
    try withoutExplictCloseTag <|>  withExplicitCloseTag

withExplicitCloseTag :: Parser XML_AST
withExplicitCloseTag = 
  do
    (tag,a) <- openTag
    innerXML <- many innerXML
    closeTag tag
    return (Node tag a innerXML)


innerXML = parseXML <|> getBody

openTag :: Parser (String, [(String,String)])
openTag =
  do
    try $ char '<' >> notFollowedBy (char '/')
    tag <- many (letter <|> digit)
    spaces
    a <- try (many keyValue)
    char '>'
    return (tag, a)

closeTag :: String -> Parser ()
closeTag str =
  do
    try $ string "</"
    spaces
    string str
    spaces
    char '>'
    return ()

withoutExplictCloseTag :: Parser XML_AST
withoutExplictCloseTag = 
  do
    try $ char '<' >> notFollowedBy (char '/')
    tag <- many (letter <|> digit)
    spaces
    a <- try (many keyValue)
    spaces
    string "/>"
    return (Node tag a [])
                   
keyValue :: Parser (String, String)
keyValue = 
  do
    key <- many1 (letter <|> digit)
    spaces
    char '='
    spaces
    value <- quotedString
    spaces
    return (key, value)

quotedString :: Parser String
quotedString = 
  do 
    c <- (try (char '"')) <|> char '\''
    content <- many (quotedChar c)
    char c <?> "quote at end"
    return content

quotedChar :: Char -> Parser Char
quotedChar c =
  try (string ['\\', c] >> return c)        
  <|> noneOf [c]

stripLeadingSpaces :: String -> String
stripLeadingSpaces str =
  let newStr = parse (spaces >> getInput >>= \r -> return r) "" str
  in case newStr of
    Right s -> s
    _ -> str
    

main = do
     str <- readFile "test.xml"
     let n = parse parseXML "" (stripLeadingSpaces str)
     putStrLn (show n)
     return ()