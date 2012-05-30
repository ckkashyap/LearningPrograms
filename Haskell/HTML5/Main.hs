import Dancer
import JavaScript
import Control.Monad.State

type MyState = State [Dancer] Int

dancer = Dancer {
     neckAngle          = (0,90, 90),
     backBoneAngle      = (0, 90, 90),
     backBonePosition   = (0, 0, 0),
     upperLeftArmAngle  = (0, 90, 210),
     upperRightArmAngle = (0, 90, -30),
     lowerLeftArmAngle  = (0, 90, -90),
     lowerRightArmAngle = (0, 90, -90),
     upperLeftLegAngle  = (0, 90, 210),
     upperRightLegAngle = (0, 90, -30),
     lowerLeftLegAngle  = (0, 90, -90),
     lowerRightLegAngle = (0, 90, -90)
       }

liftLeftUpperArm :: Double -> Dancer -> Dancer
liftLeftUpperArm x d = d {upperLeftArmAngle = (xa, ya, (za + x))} where
             (xa, ya, za) = upperLeftArmAngle d

liftRightUpperArm :: Double -> Dancer -> Dancer
liftRightUpperArm x d = d {upperRightArmAngle = (xa, ya, (za - x))} where
             (xa, ya, za) = upperRightArmAngle d




applyChange f = do
             (step:steps) <- get
             let newStep = f step
             put (newStep:step:steps)
             return 0

dance = do
      applyChange ((liftLeftUpperArm 10).(liftRightUpperArm 10))



hello = execState dance [dancer]
javascript = animation2JS (map dancer2triangles hello)




main = do
     writeFile "actionList.js" javascript
     putStrLn "Done"