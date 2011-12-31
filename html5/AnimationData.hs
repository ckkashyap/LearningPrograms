module AnimationData where
import Control.Monad.State

type Point = (Double,Double,Double)

data Axis = X|Y|Z
     deriving (Show)

data Direction = Right | Left | Up | Down

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

