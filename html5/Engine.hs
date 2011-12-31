{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad.State

type Point3D       = (Double, Double, Double)
type Point2D       = (Int, Int)
type Angle         = Double
type Radian        = Double
type LineSegment3D = (Point3D, Point3D)
type LineSegment2D = (Point2D, Point2D)


data Axis = X|Y|Z deriving (Show)

rotate :: Point3D -> Angle -> Axis -> Point3D
rotate (x,y,z) angle axis = (x', y', z')
       where
        (x',y',z') = case axis of
           X -> (x, f1 y z, f2 y z)
           Y -> (f2 z x, y, f1 z x)
           Z -> (f1 x y, f2 x y, z) 
        f1 a b = (a * cos rads) - (b * sin rads)
        f2 a b = (a * sin rads) + (b * cos rads)
        rads   = pi * angle / 180.0
        

to2D :: Point3D -> Point2D
to2D (x,y,z) = (x', y')
     where
        x'      = round (((distance*x)/factor))
        y'      = round (((distance*y)/factor))
        factor  = if factor' == 0 then 0.0001 else factor'
        factor' = distance + z

to2DSegment :: LineSegment3D -> LineSegment2D
to2DSegment (p1,p2) = (to2D p1, to2D p2)


distance = 50
zCoordinate = 25

cube :: [LineSegment3D]
cube = [
     ((-50,-50, zCoordinate), (50, -50, zCoordinate)),
     ((50,-50, zCoordinate), (50, 50, zCoordinate)),
     ((50,50, zCoordinate), (-50, 50, zCoordinate)),
     ((-50,50, zCoordinate), (-50, -50, zCoordinate)),

     ((-50,-50, zCoordinate+50), (50, -50, zCoordinate+50)),
     ((50,-50, zCoordinate+50), (50, 50, zCoordinate+50)),
     ((50,50, zCoordinate+50), (-50, 50, zCoordinate+50)),
     ((-50,50, zCoordinate+50), (-50, -50, zCoordinate+50))

     ] 

segments2script :: [LineSegment2D] -> String
segments2script [] = ""
segments2script (((x1,y1),(x2,y2)):ss) =  "context.moveTo(" ++ (show x1) ++ ", " ++ (show y1) ++ ");\n" ++ "context.lineTo(" ++ (show x2) ++ ", " ++ (show y2) ++ ");\n" ++ segments2script ss


getJS lineSegments = "var actionList = [\n" ++ (foldr g [] (map f ps)) ++ "\n]\n"
      where
        line




