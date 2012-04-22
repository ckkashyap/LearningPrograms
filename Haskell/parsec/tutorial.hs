{-

        http://legacy.cs.uu.nl/daan/download/parsec/parsec.html

-}



import Text.ParserCombinators.Parsec



simple :: Parser Char
simple = letter

run :: (Show a) => Parser a -> String -> IO ()
run p input = case (parse p "" input) of
    Left err -> do {
         putStr "Parser error at ";
         print err;
         }
    Right x -> print x


openClose :: Parser Char
openClose = do
          char '('
          char ')'
          

parens :: Parser ()
parens = do
       char '('
       parens
       char ')'
       parens
       <|> return ()
       








