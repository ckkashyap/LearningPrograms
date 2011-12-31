module Trigonometry where
import AnimationData
import Config

angle2radians :: Angle -> Radian
angle2radians a = pi * a / 180.0

radians2angle :: Radian -> Angle
radians2angle a = 180.0 * a / pi

rotate :: Point -> Angle -> Axis -> Point
rotate (x,y,z) angle X = (x',y',z')
       where
        x' = x
        y' = (y * cos rads) - (z * sin rads)
        z' = (y * sin rads) + (z * cos rads)
        rads = angle2radians angle
rotate (x,y,z) angle Y = (x',y',z')
       where
        x' = (z * sin rads) + (x * cos rads)
        y' = y
        z' = (z * cos rads) - (x * sin rads)
        rads = angle2radians angle
rotate (x,y,z) angle Z = (x',y',z')
       where
        x' = (x * cos rads) - (y * sin rads)
        y' = (x * sin rads) + (y * cos rads)
        z' = z
        rads = angle2radians angle

mapPointOnScreen :: Point -> (Int,Int)
mapPointOnScreen (x,y,z) = if z > 0 then
                              (300 + (round (((distance*x)/(distance-z)))), 300 + (round (((distance*y)/(distance-z)))))
                              --(300 + (round (((zoom*x)/(z)))), 300 + (round (((zoom*y)/(z)))))
                           else
                                (0,0)
