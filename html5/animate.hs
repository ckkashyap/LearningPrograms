{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad.State

type Point = (Double,Double,Double)
data Axis = X|Y|Z
     deriving (Show)

type Angle = Double

data Position = Position {
                shoulder :: (Angle,Angle,Angle),
                neck :: (Angle,Angle,Angle),
                leftUpperLeg :: (Angle,Angle,Angle),
                rightUpperLeg :: (Angle, Angle, Angle),
                leftKnee :: Angle,
                rightKnee :: Angle,
                leftUpperHand :: (Angle,Angle,Angle),
                rightUpperHand :: (Angle,Angle,Angle),
                leftElbow :: Angle,
                rightElbow :: Angle
      }
     deriving (Show)

type MyState a = State [Position] a
     
move :: MyState ()
move = do
     p <- get
     return ()


rotate :: Point -> Axis -> Point
rotate (x,y,z) X = 



--positionToJavaScriptFunction :: Position -> String
--positionToJavaScriptFunction (Position {shoulder, neck, leftUpperLeg, rightUpperLeg, leftKnee, rightKnee, leftUpperHand, rightUpperHand, leftElbow, rightElbow}) = ""
--                             where
--                                leftShoulderX = 
--


head = 3
body = 5
upperArm = 2
foreArm = 3
upperLeg = 3
lowerLeg = 5

ground = -5

initPosition :: Position
initPosition = Position {
                shoulder = (90,0,90),
                neck = (90,0,90),
                leftUpperLeg = (225,0,90),
                rightUpperLeg = (315, 0, 90),
                leftKnee = 5,
                rightKnee = 5,
                leftUpperHand = (225,0,90),
                rightUpperHand = (315,0,90),
                leftElbow = 5,
                rightElbow = 5
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