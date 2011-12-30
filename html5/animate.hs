{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad.State

type Point = (Double,Double,Double)
data Axis = X|Y|Z
     deriving (Show)

type Angle = Double
type Radian = Double
type Segment = (Point,Point)

data Position = Position {
                shoulder :: (Angle,Angle,Angle),
                neck :: (Angle,Angle,Angle),
                leftUpperLeg :: (Angle,Angle,Angle),
                rightUpperLeg :: (Angle, Angle, Angle),
                leftKnee :: Angle,
                rightKnee :: Angle,
                leftUpperArm :: (Angle,Angle,Angle),
                rightUpperArm :: (Angle,Angle,Angle),
                leftElbow :: Angle,
                rightElbow :: Angle,
                hip :: Point
      }
     deriving (Show)

type MyState a = State [Position] a

move :: MyState ()
move = do
     p <- get
     return ()


angle2radians :: Angle -> Radian
angle2radians a = pi * a / 180.0

radians2angle :: Radian -> Angle
radians2angle a = 180.0 * a / pi


rotate :: Point -> Angle -> Axis -> Point
rotate (x,y,z) angle X = (x',y',z')
       where
        x' = x
        y' = (y * cos rads) - (z * sin rads)
        z' = (y * sin rads) - (z * cos rads)
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



toSegments :: Position -> [Segment]
toSegments (Position {
               shoulder=(sx,sy,sz),
               neck =  (nx,ny,nz),
               leftUpperLeg = (lulx,luly,lulz),
               rightUpperLeg = (rulx,ruly,rulz),
               leftKnee = lk,
               rightKnee = rk,
               leftUpperArm = (luax,luay,luaz),
               rightUpperArm = (ruax, ruay, ruaz),
               leftElbow = le,
               rightElbow = re,
               hip = (hx,hy,hz)               
               })
               = [(p1,p2), (p2,p3), (p3,p1), (p1,p4), (p4,p5), (p1,p6), (p6,p7), (p2,p8), (p8,p9), (p3,p10), (p10,p11)]
               where 
                     p1 = (hx,hy,hz)
                     p2 = applyRotations ((hx-(bodySize/2)), hy - bodySize, hz) sx sy sz
                     p3 = applyRotations ((hx+(bodySize/2)), hy - bodySize, hz) sx sy sz

                     p4 = applyRotations (hx,hy+upperLegSize,hz) lulx (270-luly) lulz
                     p5 = applyRotations (hx,hy+upperLegSize+lowerLegSize,hz) lulx (270 - luly) (lulz + lk)
                     p6 = applyRotations (hx,hy+upperLegSize,hz) rulx (270 + ruly) rulz
                     p7 = applyRotations (hx,hy+upperLegSize+lowerLegSize,hz) rulx (270 + ruly) (rulz + rk)

                     p8 = applyRotations (((hx-(bodySize/2)), hy - bodySize + upperArmSize , hz)) luax (270 - luay) luaz
                     p9 = applyRotations (((hx-(bodySize/2)), hy - bodySize + upperArmSize + foreArmSize, hz)) luax (270 - luay) (luaz + le)
                     p10 = applyRotations (((hx-(bodySize/2)), hy - bodySize + upperArmSize , hz)) ruax (270 - ruay) ruaz
                     p11 = applyRotations (((hx-(bodySize/2)), hy - bodySize + upperArmSize + foreArmSize, hz)) ruax (270 - ruay) (ruaz + re)
        
                     applyRotations p xAngle yAngle zAngle = (rotate (rotate (rotate p xAngle X) yAngle Y) zAngle Z)






positionToJavaScriptFunction :: Position -> String
positionToJavaScriptFunction (Position {shoulder=(x,y,z), neck, leftUpperLeg, rightUpperLeg, leftKnee, rightKnee, leftUpperArm, rightUpperArm, leftElbow, rightElbow}) = ""


headSize = 3
bodySize = 5
upperArmSize = 2
foreArmSize = 3
upperLegSize = 3
lowerLegSize = 5

groundY = 0

initPosition :: Position
initPosition = Position {
                shoulder = (90,0,90),
                neck = (90,0,90),
                leftUpperLeg = (225,0,90),
                rightUpperLeg = (315, 0, 90),
                leftKnee = 85,
                rightKnee = 85,
                leftUpperArm = (225,0,90),
                rightUpperArm = (315,0,90),
                leftElbow = 85,
                rightElbow = 85,
                hip = (0,0,0)
             }

getPositionList :: MyState Position -> [Position]
getPositionList s = reverse ps
                where
                        (a,ps) = runState s [initPosition]

          
--
--
--
--onList animation
--