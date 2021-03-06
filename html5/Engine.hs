{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad.State

type Point3D       = (Double, Double, Double)
type Point2D       = (Int, Int)
type Angle         = Double
type Radian        = Double
type LineSegment3D = (Point3D, Point3D)
type LineSegment2D = (Point2D, Point2D)
type Object        = [LineSegment3D]

data Axis = X|Y|Z deriving (Show)

rotate :: Point3D -> Angle -> Axis -> Point3D
rotate (x,y,z) angle axis = (x', y', z')
       where
        (x',y',z') = case axis of
--           X -> (x, (f1 y (z + zCoordinate)) - zCoordinate, (f2 y (z + zCoordinate)) - zCoordinate)
           X -> (x, (f1 y z), (f2 y z))
           Y -> (f2 z x, y, f1 z x)
           Z -> (f1 x y, f2 x y, z) 
        f1 a b = (a * cos rads) - (b * sin rads)
        f2 a b = (a * sin rads) + (b * cos rads)
        rads   = pi * angle / 180.0
        

to2D :: Point3D -> Point2D
to2D (x,y,z) = (x'+300, y'+300)
     where
        x'      = round (((distance*x)/factor))
        y'      = round (((distance*y)/factor))
        factor  = if factor' == 0 then 0.0001 else factor'
        factor' = distance + z

to2DSegment :: LineSegment3D -> LineSegment2D
to2DSegment (p1,p2) = (to2D p1, to2D p2)


distance = 150
zCoordinate = 0

squareAtZ z = [
     ((-50,-50, zCoordinate - z), (50, -50, zCoordinate - z)),
     ((50,-50, zCoordinate - z), (50, 50, zCoordinate - z)),
     ((50,50, zCoordinate - z), (-50, 50, zCoordinate - z)),
     ((-50,50, zCoordinate - z), (-50, -50, zCoordinate - z))
     ]    

squarePipe = concat $ map squareAtZ [ i*10 | i <- [-15 .. 15] ]



cube :: Object
cube = [
     ((-50,-50, zCoordinate-20), (50, -50, zCoordinate-20)),
     ((50,-50, zCoordinate-20), (50, 50, zCoordinate-20)),
     ((50,50, zCoordinate-20), (-50, 50, zCoordinate-20)),
     ((-50,50, zCoordinate-20), (-50, -50, zCoordinate-20)),

     ((-50,-50, zCoordinate), (50, -50, zCoordinate)),
     ((50,-50, zCoordinate), (50, 50, zCoordinate)),
     ((50,50, zCoordinate), (-50, 50, zCoordinate)),
     ((-50,50, zCoordinate), (-50, -50, zCoordinate)),


     ((-50,-50, zCoordinate+20), (50, -50, zCoordinate+20)),
     ((50,-50, zCoordinate+20), (50, 50, zCoordinate+20)),
     ((50,50, zCoordinate+20), (-50, 50, zCoordinate+20)),
     ((-50,50, zCoordinate+20), (-50, -50, zCoordinate+20))
     ] 

objectToJSFunction :: Object -> String
objectToJSFunction object = "function (context) {\n" ++ (objectToJSFunction' object2D)  ++ "\n}\n"
 where
  object2D                                      = map to2DSegment object
  objectToJSFunction' []                        = ""
  objectToJSFunction' (((x1,y1),(x2,y2)):lines) = "context.moveTo(" ++ (show x1) ++ ", " ++ (show y1) ++ ");\n" ++ "context.lineTo(" ++ (show x2) ++ ", " ++ (show y2) ++ ");\n" ++ objectToJSFunction' lines

getJS :: [Object] -> String
getJS objects = "var actionList = [\n" ++ (foldr g [] (map objectToJSFunction objects)) ++ "\n]\n"
      where
        g scr [] = scr
        g scr rest = scr ++ "," ++ rest

rotateObject :: Angle -> Axis -> Object -> Object
rotateObject angle axis = map f
             where
                f (p1,p2) = (rotate p1 angle axis, rotate p2 angle axis)


getObjectList :: [Angle] -> [Object]
getObjectList [] = []
getObjectList (i:is) = (rotateObject i Y squarePipe) : getObjectList is

angles :: [Angle]
angles = [0..360]

main = do
  writeFile "actionList.js" (getJS (getObjectList angles))
  return ()