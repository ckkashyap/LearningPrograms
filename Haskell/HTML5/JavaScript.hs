module JavaScript where

import qualified Geometry as Geometry

data JSFunction = MoveTo | LineTo

jsFunction :: JSFunction -> String
jsFunction j = case j of 
                    MoveTo -> "moveTo"
                    LineTo -> "lineTo"

point2js :: JSFunction -> Geometry.Point2D -> String
point2js jsf (x,y) = f ++ "(" ++ (show x) ++ ", " ++ (show y) ++ ");\n"
         where f = "context." ++ jsFunction jsf

triangle2js :: Geometry.Triangle2D -> String
triangle2js (p1, p2, p3) = foldr (\x y -> (s x) ++ y) "" [p1, p2, p3]
            where s p = (point2js MoveTo p) ++ (point2js LineTo p)
            


test_points = map (\x -> (x,x+1)) [1,2,3] :: [Geometry.Point2D]

test_triangle = (p1,p2,p3) where
              [p1, p2, p3] = test_points
              
test_js = triangle2js test_triangle
