
  parseCore :: String -> CoreProgram
  parseCore input = case parse pProgram "" input of
                      Left v -> error $ show v
                      Right v -> v
 
 
 
 
 
 
 
 
 
 
  lexer     = P.makeTokenParser coreDef
 
  coreDef    = emptyDef
                 { P.commentLine    = "||"
                 , P.identStart     = letter
                 , P.identLetter    = alphaNum <|> char '_'
                 , P.reservedNames  = ["let","in","letrec","case","of","Pack"]
                 }
 
 
  parens          = P.parens lexer    
  braces          = P.braces lexer
  angles          = P.angles lexer
  semiSep1        = P.semiSep1 lexer
  semiSep         = P.semiSep lexer
  semi            = P.semi lexer
  whiteSpace      = P.whiteSpace lexer    
  symbol          = P.symbol lexer    
  identifier      = P.identifier lexer    
  reserved        = P.reserved lexer    
  natural         = P.natural lexer
  comma           = P.comma lexer
 
 
 
 
  pProgram :: Parser CoreProgram
  pProgram = whiteSpace >> (semiSep1 pSc <* eof)
 
  pSc :: Parser CoreScDefn
  pSc = do
    var <- identifier
    args <- many identifier
    symbol "="
    expr <- pExpr
    return (var, args, expr)
