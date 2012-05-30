import Dancer
import JavaScript


dancer = Dancer {
     neckAngle = (0,90, 90),
     backBoneAngle = (0, 90, 90),
     backBonePosition = (0, 0, 0),
     upperLeftArmAngle = (0, 90, 210),
     upperRightArmAngle = (0, 90, -30),
     lowerLeftArmAngle = (0, 90, -90),
     lowerRightArmAngle = (0, 90, -90),
     upperLeftLegAngle = (0, 90, 210),
     upperRightLegAngle = (0, 90, -30),
     lowerLeftLegAngle = (0, 90, -90),
     lowerRightLegAngle = (0, 90, -90)
       }

ts = dancer2triangles dancer

javascript = animation2JS [ts]

main = do
     writeFile "actionList.js" javascript
     putStrLn (show (length ts))
     putStrLn "Done"