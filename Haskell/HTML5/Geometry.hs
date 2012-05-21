module Geometry where

type Point3D    = (Double, Double, Double)
type Point2D    = (Int, Int)
type Angle      = Double
type Radian     = Double
type Triangle3D = (Point3D, Point3D, Point3D)
type Triangle2D = (Point2D, Point2D, Point2D)
type Frame      = [TransformedTriangle]
type Animation  = [Frame]
type Rotation   = (Angle, Angle, Angle)
type Translation = (Double, Double, Double)

data TransformedTriangle = TransformedTriangle Triangle3D Rotation Translation

data Axis = X | Y | Z deriving (Show)

rotatePoint :: Angle -> Axis -> Point3D -> Point3D
rotatePoint angle axis (x,y,z) = (x', y', z')
       where
        (x',y',z') = case axis of
           X -> (x, (f1 y z), (f2 y z))
           Y -> (f2 z x, y, f1 z x)
           Z -> (f1 x y, f2 x y, z) 
        f1 a b = (a * cos rads) - (b * sin rads)
        f2 a b = (a * sin rads) + (b * cos rads)
        rads   = pi * angle / 180.0
        
translatePoint :: Double -> Axis -> Point3D -> Point3D
translatePoint distance axis (x, y, z) = 
               case axis of
                    X -> (x + distance, y, z)
                    Y -> (x, y + distance, z)
                    Z -> (x, y, z + distance)

rotateTriangle :: Angle -> Axis -> Triangle3D -> Triangle3D
rotateTriangle angle axis (p1, p2, p3) = (p1', p2', p3') where
               [p1', p2', p3'] = map (rotatePoint angle axis) [p1, p2, p3]

translateTriangle :: Double -> Axis -> Triangle3D -> Triangle3D
translateTriangle distance axis (p1, p2, p3) = (p1', p2', p3') where
                  [p1', p2', p3'] = map (translatePoint distance axis) [p1, p2, p3]

project :: Point3D -> Point2D
project (x,y,z) = (x'+300, y'+300)
     where
        x'       = round (((distance*x)/factor))
        y'       = round (((distance*y)/factor))
        factor   = if factor' == 0 then 0.0001 else factor'
        factor'  = distance + z
        distance = 150

projectTriangle :: Triangle3D -> Triangle2D
projectTriangle (p1, p2, p3) = (p1', p2', p3') where
                [p1', p2', p3'] = map project [p1, p2, p3]