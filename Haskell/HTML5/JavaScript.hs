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
triangle2js (p1, p2, p3) = foldr (\(_p1, _p2) y -> (s _p1 _p2) ++ y) "" linePointsList
            where s __p1 __p2      = (point2js MoveTo __p1) ++ (point2js LineTo __p2)
                  list             = cycle [p1, p2, p3]
                  listShiftedByOne = tail list
                  linePointsList   = take 3 (zip list listShiftedByOne)
                  
            
object2JS :: Geometry.Scene -> String
object2JS object = "function (context) {\n" ++ (object2JS' object2D)  ++ "}\n"
 where
  object2D = map Geometry.projectTriangle object
  object2JS' [] = ""
  object2JS' (t:ts) = triangle2js t ++ object2JS' ts

animation2JS :: Geometry.Animation -> String
animation2JS animation = "var actionList = [\n" ++ (foldr f [] (map object2JS animation)) ++ "]\n" where
             f sceneJS []   = sceneJS
             f sceneJS rest =  sceneJS ++ ",\n" ++ rest


test_points3D = map (\x -> (x,x+1,x-1)) [1,2,3] :: [Geometry.Point3D]

test_triangle3D = (p1,p2,p3) where
              [p1, p2, p3] = test_points3D
              
test_scene = [test_triangle3D]

test_animation = take 2 (repeat test_scene)



test_js = animation2JS test_animation
