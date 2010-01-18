--------------------------------------------------------------------------------
-- Region

-- This one doesn't work properly -
-- CPoints are difficult to manipulate as strings, hence the `hasVar`
-- problems, it gives some idea of the method though.



module Region where

import Data.Char ( isAlpha )
import Data.List ( foldl' )


-- Prolog
type CExpr = String
type CPred = String
type CFloat = Float

infixr 6 ++&
(++&) :: Show a => String -> a -> String
s ++& a = s ++ show a


sqrdist _ = ""

add :: CPoint -> CPoint -> CPoint
add a b = a ++ "+" ++ b

sub :: CPoint -> CPoint -> CPoint
sub a b = a ++ "-" ++ b

hasVar :: CExpr -> Bool
hasVar = any isAlpha

cfst :: CPoint -> CExpr
cfst a | hasVar a  = a ++ ".x"
      | otherwise  = "1.1"

csnd :: CPoint -> CExpr
csnd a | hasVar a  = a ++".y"
      | otherwise  = "2.2"

pt :: (CFloat,CFloat) -> CPoint
pt = show

intersect :: [Region] -> Region
intersect (r:rs) = foldl' (/\) r rs
intersect []    = error $ "intersect on empty list"



-- presentation

type CPoint = CExpr
type Region = CPoint -> CPred


circle :: CFloat -> Region
circle n = \p -> "(" ++ sqrdist p ++ "<" ++& n ++ "*" ++& n ++ ")"

halfplane :: CPoint -> CPoint -> Region
halfplane a b = \p -> "(" ++ zcross (a `sub` p) (b `sub` a) ++ " > 0.0)"
  where
    zcross e1 e2 =
      "(" ++ cfst e1 ++ "*" ++ csnd e2 ++ "-" ++ csnd e2 ++ "*" ++
cfst e1 ++ ")"


(/\) :: Region -> Region -> Region
r1 /\ r2 = \p -> "(" ++ r1 p ++ " && " ++ r2 p ++ ")"

(\/) :: Region -> Region -> Region
r1 \/ r2 = \p -> "(" ++ r1 p ++ " || " ++ r2 p ++ ")"

at :: Region -> CPoint -> Region
r `at` p0 = \p -> r (p `sub` p0)

convexPoly :: [CPoint] -> Region
convexPoly (p:ps) =
  intersect (zipWith halfplane ([p] ++ ps) (ps ++ [p]))


tightZone :: CPoint -> CPred
tightZone =
  (convexPoly [pt (0.0,5.0), pt (118.0,32.0),
              pt (118.0,62.0), pt (0.0,25.0) ])
    \/
  (convexPoly [pt (118.0,32.0), pt (259.0,5.0),
              pt (259.0, 25.0), pt (118.0,62.0)])


main = tightZone e1 where
    e1::CExpr
    e1 = "p"
