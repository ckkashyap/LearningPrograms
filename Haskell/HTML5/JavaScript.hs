module JavaScript where

import qualified Geometry as Geometry

data JSFunction = MoveTo | LineTo

jsFunction :: JSFunction -> String
jsFunction j = case j of 
                    MoveTo -> "moveTo"
                    LineTo -> "lineTo"

point2js :: JSFunction -> Geometry.Point2D -> String
point2js jsf (x,y) = f ++ "(" ++ (show x) ++ ", " ++ (show y) ++ ");\n"
         where f = "\tctx." ++ jsFunction jsf

triangle2js :: Geometry.Triangle2D -> String
triangle2js (p1, p2, p3) = triangle2js' (take 4 (cycle [p1, p2, p3])) where
            triangle2js' (_p1:_p2:[]) = str _p1 _p2
            triangle2js' (_p1:_p2:ps) = (str _p1 _p2) ++ (triangle2js' (_p2:ps))
            str _p1 _p2               = (point2js MoveTo _p1) ++ (point2js LineTo _p2)

object2JS :: Geometry.Scene -> String
object2JS object = "function (ctx) {\n" ++ (object2JS' object2D)  ++ "}\n"
 where
  object2D = map Geometry.projectTriangle object
  object2JS' [] = ""
  object2JS' (t:ts) = triangle2js t ++ object2JS' ts



animation2JS :: Geometry.Animation -> String
animation2JS animation = "var actionList = [\n" ++ (foldr f [] (map object2JS animation)) ++ "]\n" where
             f sceneJS []   = sceneJS
             f sceneJS rest =  sceneJS ++ ",\n" ++ rest


test_points = map (\i -> (i,i+1,i-1)) [1..3] :: [Geometry.Point3D]

test_triangle = (p1, p2, p3) where [p1, p2, p3] = test_points

test_scene = [test_triangle] :: Geometry.Scene

test_animation = take 2 $ cycle [test_scene] :: Geometry.Animation

output = putStr $ animation2JS test_animation