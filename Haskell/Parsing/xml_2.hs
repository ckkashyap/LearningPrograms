import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>),many)

data XML =  Node String [XML]
          | Body String deriving Show


gettext = Body <$> many1 (noneOf "><")

xml :: Parser XML
xml = do
    tag <- openTag
    innerXML <- many innerXML
    endTag tag
    return (Node tag innerXML) 

innerXML = xml <|> gettext

openTag :: Parser String
openTag = try (char '<' *> many1 (noneOf "/>")) <* char '>'

endTag :: String -> Parser String
endTag str = string "</" *> string str <* char '>'


h1 = parse xml "" "<a>A</a>"
h2 = parse xml "" "<a><b>A</b></a>"
h3 = parse xml "" "<a><b><c></c></b></a>"
h4 = parse xml "" "<a><b></b><c></c>HELLO</a>"
h5 = parse xml "" "<a><b></b><c></c>HELLO</a>abcd"

