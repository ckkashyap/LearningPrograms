import Data.Char

type Parser a = String -> Consumed a

data Consumed a = 
     Consumed (Reply a)
   | Empty (Reply a) 
   deriving (Show)

data Reply a = 
     Ok a String 
   | Error
   deriving (Show)


return x = \input -> Empty (Ok x input)

satisfy :: (Char -> Bool) -> Parser Char
satisfy test
        = \input -> case (input) of
          []     -> Empty Error
          (c:cs) | test c -> Consumed (Ok c cs)
                 | otherwise -> Empty Error


char c = satisfy (==c)
letter = satisfy isAlpha
digit  = satisfy isDigit

    
(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= f =
  \input -> case (p input) of
         Empty reply1 -> case reply1 of
               Ok x rest -> ((f x) rest)
               Error     -> Empty Error
         
         Consumed reply1 -> Consumed
                  (case reply1 of
                        Ok x rest -> case ((f x) rest) of
                                          Consumed reply2 -> reply2
                                          Empty reply2    -> reply2
                        Error -> Error
                   )
               

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q =
  \input -> case (p input) of
         Empty Error -> q input
         Empty ok    -> case (q input) of
                     Empty _ -> Empty ok
                     Consumed _ -> Consumed ok
         Consumed ok -> Consumed ok