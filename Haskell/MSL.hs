module MSL where


type Expr = String
type Predicate = Expr
type Statement = String
type Fieldname = String

data Bitsource = Source Expr Expr
  deriving Show


newbitsource a i  = Source a i

initbs (Source _ i) =  i ++ " = 0;"

getByte (Source a i)  =  a ++ "[" ++  i ++ "/8]"

getNthByte :: Bitsource -> Int -> Expr
getNthByte (Source a i) n
    | n == 0    = a ++ "[" ++  i ++ "/8]"
    | otherwise = a ++ "[" ++  i ++ "/8+" ++ show n ++ "]"

advanceByte (Source a i) = i ++ " = " ++ i ++ "-(" ++ i ++ "%8)+8;"

advanceNBytes (Source a i) n
    | n == 0    = ""
    | otherwise = i ++ " = " ++ i ++ "-(" ++ i ++ "%8)+(8*" ++ show n++");"


data Recordfield = Field Expr [Fieldname]
  deriving Show

recordptr :: Expr -> Recordfield
recordptr e  = Field e []

subfield :: Recordfield -> Fieldname -> Recordfield
subfield (Field e fl) f  = Field e (f:fl)

deref :: Recordfield -> Expr
deref (Field e fl)
    = "(*" ++e++ ")" ++ concat ( map cojoin (reverse fl) )
  where
    cojoin :: Fieldname -> String
    cojoin s = "." ++ s    



type Message = Bitsource -> Recordfield -> Statement -> Statement    

infield :: Fieldname -> Message -> Message
infield f m src tgt
    = m src (subfield tgt f)


c_if :: Expr -> Statement -> Statement -> Statement
c_if e s1 s2
    = if e=="1" || e=="(1)"
        then s1
        else "if("++e++"){"
                        ++ s1
                        ++ "}" ++ if s2 /= "" then "else {" ++ s2 ++ "}" else ""


seqmsg :: [Message] -> Message
seqmsg (m:ml) src tgt s
      = (m src tgt "error_action();") ++  (seqmsg ml src tgt s)
seqmsg [] _ _ _ = ""

asc2Int :: Int -> (Int,Int) -> Message
asc2Int w (lo,hi) src tgt s
    = c_if ("inrange(" ++ (getByte src) ++ ", "
                        ++ (ms w) ++ ", " ++ (ms lo)
                        ++ ", " ++ (ms hi))
                        ""
                        s
  where
      ms n = show n    


alt :: [Message] -> Message
alt (m:ml) src tgt s
      = m src tgt (alt ml src tgt s)


delim :: Expr -> Message
delim e src tgt s
      = "if (" ++ getByte src ++ " == " ++ e ++")"
              ++ advanceByte src

rangex :: Int -> Int -> [Int]
rangex i j
        | i > j    = []
        | otherwise = (i:(rangex (i+1) j))


c_and [] =  ""
c_and [pred] = "(" ++ pred ++ ")"    
c_and (pred1:pred2:preds) = "(" ++ pred1 ++ " && " ++ c_and (pred2:preds) ++ ")"

asc :: String -> String -> Message
asc chars value src tgt s
      = c_if ""
            (deref tgt ++ " == " ++ value ++ ";" )
            s

skip :: Int -> Message
skip n src tgt s
      = (deref tgt) ++ "= 1;"
                    ++ (advanceNBytes src n)

--------------------------------------------------------------------------------

bs = newbitsource "A" "bit"
f = recordptr "target"


main = delim "6" bs f "abort();"


to_confidence = alt [ asc "HH" "High"
                    , asc "MM" "Medium"
                    , asc "LL" "Low"
                    , asc "NN" "None"
                    ]    


