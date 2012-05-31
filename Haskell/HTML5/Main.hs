import Dancer
import JavaScript
import Control.Monad.State

type MyState = State [Dancer] Int

dancer = Dancer {
       neckAngleX = 0,
       neckAngleY = 90,
       neckAngleZ =  90,

       backBoneAngleX = 0,
       backBoneAngleY =  90,
       backBoneAngleZ =  90,

       backBonePositionX = 0,
       backBonePositionY =  0,
       backBonePositionZ =  0,

       upperLeftArmAngleX = 0,
       upperLeftArmAngleY =  90,
       upperLeftArmAngleZ =  210,

       upperRightArmAngleX = 0,
       upperRightArmAngleY =  90,
       upperRightArmAngleZ =  -30,

       lowerLeftArmAngleX = 0,
       lowerLeftArmAngleY =  90,
       lowerLeftArmAngleZ =  -90,

       lowerRightArmAngleX = 0,
       lowerRightArmAngleY =  90,
       lowerRightArmAngleZ =  -90,

       upperLeftLegAngleX = 0,
       upperLeftLegAngleY =  90,
       upperLeftLegAngleZ =  210,

       upperRightLegAngleX = 0,
       upperRightLegAngleY =  90,
       upperRightLegAngleZ =  -30,

       lowerLeftLegAngleX = 0,
       lowerLeftLegAngleY =  90,
       lowerLeftLegAngleZ =  -90,

       lowerRightLegAngleX = 0,
       lowerRightLegAngleY =  90,
       lowerRightLegAngleZ =  -90

       }



liftLeftUpperArm :: Double -> Dancer -> Dancer
liftLeftUpperArm x d = d {upperLeftArmAngleZ = (upperLeftArmAngleZ d) + x}


liftRightUpperArm :: Double -> Dancer -> Dancer
liftRightUpperArm x d = d {upperRightArmAngleZ = (upperRightArmAngleZ d) - x}


turn :: Double -> Dancer -> Dancer
turn x d = d {
       neckAngleY = (neckAngleY d) - x,
       backBoneAngleY = (backBoneAngleY d) - x,
       upperLeftArmAngleY = (upperLeftArmAngleY d) - x,
       upperRightArmAngleY = (upperRightArmAngleY d) - x,
       lowerLeftArmAngleY = (lowerLeftArmAngleY d) - x,
       lowerRightArmAngleY = (lowerRightArmAngleY d) - x,
       upperLeftLegAngleY = (upperLeftLegAngleY d) - x,
       upperRightLegAngleY = (upperRightLegAngleY d) - x,
       lowerLeftLegAngleY = (lowerLeftLegAngleY d) - x,
       lowerRightLegAngleY = (lowerRightLegAngleY d) - x
          }




applyChange f = do
             (step:steps) <- get
             let newStep = f step
             put (newStep:step:steps)
             return 0

dance = do
--      applyChange ((liftLeftUpperArm 10).(liftRightUpperArm 10))
      applyChange id
      applyChange id
      applyChange id
      applyChange id
      applyChange id



      applyChange ((turn 20).(liftRightUpperArm 5).(liftLeftUpperArm 15))
      applyChange ((turn 20).(liftRightUpperArm 5).(liftLeftUpperArm 15))
      applyChange ((turn 20).(liftRightUpperArm 5).(liftLeftUpperArm 15))

      applyChange ((turn (-20)).(liftRightUpperArm (-5)).(liftLeftUpperArm (-15)))
      applyChange ((turn (-20)).(liftRightUpperArm (-5)).(liftLeftUpperArm (-15)))
      applyChange ((turn (-20)).(liftRightUpperArm (-5)).(liftLeftUpperArm (-15)))
      applyChange ((turn (-20)).(liftRightUpperArm (5)).(liftLeftUpperArm (15)))
      applyChange ((turn (-20)).(liftRightUpperArm (5)).(liftLeftUpperArm (15)))
      applyChange ((turn (-20)).(liftRightUpperArm (5)).(liftLeftUpperArm (15)))

      applyChange ((turn 20).(liftRightUpperArm (-5)).(liftLeftUpperArm (-15)))
      applyChange ((turn 20).(liftRightUpperArm (-5)).(liftLeftUpperArm (-15)))
      applyChange ((turn 20).(liftRightUpperArm (-5)).(liftLeftUpperArm (-15)))

--      applyChange ((liftLeftUpperArm 10).(liftRightUpperArm 10))



hello = execState dance [dancer]
javascript = animation2JS (map dancer2triangles hello)




main = do
     writeFile "actionList.js" javascript
     putStrLn "Done"