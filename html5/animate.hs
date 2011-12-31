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


pointGivenDistanceAndAngles :: Point -> Double -> Angle -> Angle -> Angle -> Point
pointGivenDistanceAndAngles (x,y,z) d xa ya za = (x',y',z')
                            where
                                x' = x + d * cos(angle2radians za) * sin(angle2radians ya)
                                y' = y + d * sin(angle2radians za)
                                z' = z + d * cos(angle2radians za) * cos(angle2radians ya)

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
               = [(p1,p2), (p2,p3), (p3,p1), (p1,p4), (p4,p5), (p1,p6), (p6,p7), (p2,p8), (p8,p9), (p3,p10), (p10,p11), (p12,p13), (p13,p14), (p14,p12)]
               where 
                     p1  = (hx,hy,hz)
                     p2  = applyRotations ((hx-(bodySize/2)), hy - bodySize, hz) sx sy sz
                     p3  = applyRotations ((hx+(bodySize/2)), hy - bodySize, hz) sx sy sz

                     p4  =  applyRotations (hx,hy+upperLegSize,hz) lulx luly lulz
                     p5  =  applyRotations (hx,hy+upperLegSize+lowerLegSize,hz) (lulx + lk) luly lulz
                     p6  =  applyRotations (hx,hy+upperLegSize,hz) rulx ruly rulz
                     p7  =  applyRotations (hx,hy+upperLegSize+lowerLegSize,hz) (rulx + rk) ruly rulz

                     p8  = applyRotations (((hx-(bodySize/2)), hy - bodySize + upperArmSize , hz)) (luax + sx) (luay + sy) (luaz + sz)
                     p9  = applyRotations (((hx-(bodySize/2)), hy - bodySize + upperArmSize + foreArmSize, hz)) (luax + sx + le) (luay + sy) (luaz + sz)
                     p10 = applyRotations (((hx+(bodySize/2)), hy - bodySize + upperArmSize , hz)) (ruax + sx) (ruay + sy) (ruaz + sz)
                     p11 = applyRotations (((hx+(bodySize/2)), hy - bodySize + upperArmSize + foreArmSize, hz)) (ruax + sx + re) (ruay + sy) (ruaz + sz)

                     p12  = middle p2 p3
                          where
                                middle (x1,y1,z1) (x2,y2,z2) = (((x2+x1)/ 2), ((y2+y1)/2), ((z2+z1)/2))
                                
                     p13  = applyRotations ((x'-(headSize)), y' - bodySize, z') (nx+sx) (ny+sy) (nz+sz)
                          where (x',y',z') = p12
                     p14  = applyRotations ((x'+(headSize)), y' - bodySize, z') (nx+sx) (ny+sy) (nz+sz)
                          where (x',y',z') = p12

        
                     applyRotations p xAngle yAngle zAngle = (rotate (rotate (rotate p zAngle Z) yAngle Y) xAngle X)




mapPointOnScreen :: Point -> (Int,Int)
mapPointOnScreen (x,y,z) = if z > 0 then
                              (300 + (round (((distance*x)/(distance-z)))), 300 + (round (((distance*y)/(distance-z)))))
                              --(300 + (round (((zoom*x)/(z)))), 300 + (round (((zoom*y)/(z)))))
                           else
                                (0,0)

convertSegmentsTo2D :: [Segment] -> [Segment2D]
convertSegmentsTo2D = foldr (\(p1,p2) rest -> (mapPointOnScreen p1, mapPointOnScreen p2):rest) []

test = toSegments initPosition
test1 = convertSegmentsTo2D test

segments2script :: [Segment2D] -> String
segments2script [] = ""
segments2script (((x1,y1),(x2,y2)):ss) =  "context.moveTo(" ++ (show x1) ++ ", " ++ (show y1) ++ ");\n" ++ "context.lineTo(" ++ (show x2) ++ ", " ++ (show y2) ++ ");\n" ++ segments2script ss



headSize = 10
bodySize = 30
upperArmSize = 20
foreArmSize = 30
upperLegSize = 30
lowerLegSize = 50


distance = 50
zoom = 50

groundY = 0

initPosition :: Position
initPosition = Position {
                shoulder = (0,0,0),
                neck = (0,0,0),
                leftUpperLeg = (0,0,0),
                rightUpperLeg = (0, 0, 0),
                leftKnee = 0,
                rightKnee = 0,
                leftUpperArm = (0,0,0),
                rightUpperArm = (0,0,0),
                leftElbow = 0,
                rightElbow = 0,
                hip = (0,0,10)
             }

getPositionList :: MyState () -> [Position]
getPositionList s = reverse ps
                where
                        (a,ps) = runState s [initPosition]

          

moveShoulderRight :: MyState ()
moveShoulderRight = do
     (p@(Position {shoulder = (x,y,z)}):ps) <- get
     let n = p { shoulder = (x,y+1,z)}
     put (n:p:ps)
     return ()

moveShoulderLeft :: MyState ()
moveShoulderLeft = do
     (p@(Position {shoulder = (x,y,z)}):ps) <- get
     let n = p { shoulder = (x,y-1,z)}
     put (n:p:ps)
     return ()



moveRightHand :: MyState ()
moveRightHand = do
     (p@(Position {rightUpperArm = (x,y,z), rightElbow = re}):ps) <- get
     let n = p { rightUpperArm = (x+1,y+1,z), rightElbow = re + 1 }
     put (n:p:ps)
     return ()

moveRightHand' :: MyState ()
moveRightHand' = do
     (p@(Position {rightUpperArm = (x,y,z), rightElbow = re}):ps) <- get
     let n = p { rightUpperArm = (x-1,y-1,z), rightElbow = re -1 }
     put (n:p:ps)
     return ()

moveLeftHand :: MyState ()
moveLeftHand = do
     (p@(Position {leftUpperArm = (x,y,z), leftElbow = le}):ps) <- get
     let n = p { leftUpperArm = (x+1,y-1,z), leftElbow = le }
     put (n:p:ps)
     return ()

moveLeftHand' :: MyState ()
moveLeftHand' = do
     (p@(Position {leftUpperArm = (x,y,z), leftElbow = le}):ps) <- get
     let n = p { leftUpperArm = (x-1,y+1,z), leftElbow = le }
     put (n:p:ps)
     return ()


dance' :: Int -> MyState () -> MyState ()
dance' 0 _ = return ()
dance' n a = do
       a
       dance' (n-1) a





dance :: MyState ()
dance = do
      dance' 10 (moveLeftHand >>  moveRightHand >> moveShoulderLeft)
      dance' 20 (moveLeftHand' >> moveRightHand' >> moveShoulderRight)
      dance' 10 (moveLeftHand >>  moveRightHand >> moveShoulderLeft)


getJS :: MyState () -> String
getJS steps = "var actionList = [\n" ++ (foldr g [] (map f ps)) ++ "\n]\n"
      where
        f = segments2script . convertSegmentsTo2D . toSegments
        ps = getPositionList steps
        g scr [] = "function (context) {\n" ++ scr ++ "\n}\n"
        g scr rest = "function (context) {\n" ++ scr ++ "\n},\n" ++ rest
      



main = do
--  putStrLn (getJS dance)
  writeFile "actionList.js" (getJS dance)
  return ()