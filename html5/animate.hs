{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad.State

type Point = (Double,Double,Double)
data Axis = X|Y|Z
     deriving (Show)

type Angle = Double
type Radian = Double
type Segment = (Point,Point)
type Segment2D = ((Int,Int),(Int,Int))

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
                     p1  = fixup (hx,hy,hz)
                     p2  = fixup $ applyRotations ((hx-(bodySize/2)), hy - bodySize, hz) sx sy sz
                     p3  = fixup $ applyRotations ((hx+(bodySize/2)), hy - bodySize, hz) sx sy sz

                     p4  = fixup $ applyRotations (hx,hy+upperLegSize,hz) lulx luly lulz
                     p5  = fixup $ applyRotations (hx,hy+upperLegSize+lowerLegSize,hz) (lulx + lk) luly lulz
                     p6  = fixup $ applyRotations (hx,hy+upperLegSize,hz) rulx ruly rulz
                     p7  = fixup $ applyRotations (hx,hy+upperLegSize+lowerLegSize,hz) (rulx + rk) ruly rulz

                     p8  = fixup $ applyRotations (((hx-(bodySize/2)), hy - bodySize + upperArmSize , hz)) luax luay luaz
                     p9  = fixup $ applyRotations (((hx-(bodySize/2)), hy - bodySize + upperArmSize + foreArmSize, hz)) (luax + le) luay luaz
                     p10 = fixup $ applyRotations (((hx+(bodySize/2)), hy - bodySize + upperArmSize , hz)) ruax ruay ruaz
                     p11 = fixup $ applyRotations (((hx+(bodySize/2)), hy - bodySize + upperArmSize + foreArmSize, hz)) (ruax + re) ruay ruaz
        
                     applyRotations p xAngle yAngle zAngle = (rotate (rotate (rotate p xAngle X) yAngle Y) zAngle Z)

                     fixup (x1,y1,z1) = (x1,y1 + (yOffset p5' p7'), z1)
                           where
                                p5' = applyRotations (hx,hy+upperLegSize+lowerLegSize,hz) lulx luly (lulz + lk)
                                p7' = applyRotations (hx,hy+upperLegSize+lowerLegSize,hz) rulx ruly (rulz + rk)

                     yOffset (x1,y1,z1) (x2,y2,z2) = if (y1 < y2) then adjust y1 else adjust y2
                             where
                                adjust yy = if yy < 0 then -yy else yy



mapPointOnScreen :: Point -> (Int,Int)
mapPointOnScreen (x,y,z) = if z > 0 then
                              (300 + (round (zoom*x/z)), 300 + (round (zoom*y/z)))
                           else
                                (0,0)

convertSegmentsTo2D :: [Segment] -> [Segment2D]
convertSegmentsTo2D = foldr (\(p1,p2) rest -> (mapPointOnScreen p1, mapPointOnScreen p2):rest) []

test = toSegments initPosition
test1 = convertSegmentsTo2D test

segments2script :: [Segment2D] -> String
segments2script [] = ""
segments2script (((x1,y1),(x2,y2)):ss) =  "context.moveTo(" ++ (show x1) ++ ", " ++ (show y1) ++ ");\n" ++ "context.lineTo(" ++ (show x2) ++ ", " ++ (show y2) ++ ");\n" ++ segments2script ss



headSize = 30
bodySize = 50
upperArmSize = 20
foreArmSize = 30
upperLegSize = 30
lowerLegSize = 50

zoom = 50

groundY = 0

initPosition :: Position
initPosition = Position {
                shoulder = (0,0,0),
                neck = (0,0,0),
                leftUpperLeg = (10,0,30),
                rightUpperLeg = (0, 0, 0),
                leftKnee = -30,
                rightKnee = 0,
                leftUpperArm = (0,0,0),
                rightUpperArm = (0,0,0),
                leftElbow = 0,
                rightElbow = 0,
                hip = (0,0,50)
             }

getPositionList :: MyState () -> [Position]
getPositionList s = reverse ps
                where
                        (a,ps) = runState s [initPosition]

          

moveShoulderRight :: MyState ()
moveShoulderRight = do
     (p@(Position {shoulder = (x,y,z)}):ps) <- get
     let n = p { shoulder = (x,y,z+10)}
     put (n:p:ps)
     return ()



moveShoulderLeft :: MyState ()
moveShoulderLeft = do
     (p@(Position {shoulder = (x,y,z)}):ps) <- get
     let n = p { shoulder = (x,y,z-10)}
     put (n:p:ps)
     return ()



dance :: MyState ()
dance = do
      --moveShoulderRight
      --moveShoulderRight
      --moveShoulderLeft
      --moveShoulderLeft
      return ()


getJS :: MyState () -> String
getJS steps = "var actionList = [\n" ++ (foldr g [] (map f ps)) ++ "\n]\n"
      where
        f = segments2script . convertSegmentsTo2D . toSegments
        ps = getPositionList steps
        g scr [] = "function (context) {\n" ++ scr ++ "\n}\n"
        g scr rest = "function (context) {\n" ++ scr ++ "\n},\n" ++ rest
      



main = do
  putStrLn (getJS dance)
  writeFile "actionList.js" (getJS dance)
  return ()