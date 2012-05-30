module Dancer where
import Geometry

type Coordinates3D = (Double, Double, Double)

data Dancer = Dancer {
     neckAngle          :: Coordinates3D,
     backBoneAngle      :: Coordinates3D,
     backBonePosition   :: Coordinates3D,
     upperLeftArmAngle  :: Coordinates3D,
     upperRightArmAngle :: Coordinates3D,
     lowerLeftArmAngle  :: Coordinates3D,
     lowerRightArmAngle :: Coordinates3D,
     upperLeftLegAngle  :: Coordinates3D,
     lowerLeftLegAngle  :: Coordinates3D,
     upperRightLegAngle :: Coordinates3D,
     lowerRightLegAngle :: Coordinates3D
     } deriving Show


otherEnd :: Point3D -> Coordinates3D -> Double -> Point3D
otherEnd (x, y, z) (xa, ya, za) d = (x', y', z') where
         x' = x + (d * (cos zr) * (sin yr))
         y' = y - (d * (sin zr)) -- WOW ... so that y grows upwards
         z' = z + (d * (cos zr) * (cos yr))
         zr = pi *  za / 180.0
         yr = pi *  ya / 180.0
         
                
--[0.0,1.0,1.0,2.0,3.0,5.0,8.0,13.0,21.0,34.0,55.0,89.0,144.0,233.0,377.0,610.0,987.0,1597.0,2584.0,4181.0]
fib :: [Double]
fib = 0:1:zipWith (+) fib (tail fib)

backBoneLength = 55
upperArmLength = 34
lowerArmLength = 21
upperLegLength = 34
lowerLegLength = 55
neckLength     = 13


dancer2triangles :: Dancer -> [Triangle3D]
dancer2triangles d = [ neck, backBone, upperLeftArm, upperRightArm, lowerLeftArm, lowerRightArm, upperLeftLeg, lowerLeftLeg, upperRightLeg, lowerRightLeg]  where
                 backBone = (backBonePosition d, backBonePosition d, backBoneTop)
                 backBoneTop = otherEnd (backBonePosition d) (backBoneAngle d) backBoneLength

                 neck = (backBoneTop, backBoneTop, neckTop)
                 neckTop = otherEnd backBoneTop (neckAngle d) neckLength

                 upperLeftArm = (backBoneTop, backBoneTop, upperLeftArmTip)
                 upperLeftArmTip = otherEnd backBoneTop (upperLeftArmAngle d) upperArmLength
                 lowerLeftArm = (upperLeftArmTip, upperLeftArmTip, lowerLeftArmTip)
                 lowerLeftArmTip = otherEnd upperLeftArmTip (lowerLeftArmAngle d) lowerArmLength

                 upperRightArm = (backBoneTop, backBoneTop, upperRightArmTip)
                 upperRightArmTip = otherEnd backBoneTop (upperRightArmAngle d) upperArmLength
                 lowerRightArm = (upperRightArmTip, upperRightArmTip, lowerRightArmTip)
                 lowerRightArmTip = otherEnd upperRightArmTip (lowerRightArmAngle d) lowerArmLength

                 upperLeftLeg = (backBonePosition d, backBonePosition d, upperLeftLegTip)
                 upperLeftLegTip = otherEnd (backBonePosition d) (upperLeftLegAngle d) upperLegLength
                 lowerLeftLeg = (upperLeftLegTip, upperLeftLegTip, lowerLeftLegTip)
                 lowerLeftLegTip = otherEnd upperLeftLegTip (lowerLeftLegAngle d) lowerLegLength

                 upperRightLeg = (backBonePosition d, backBonePosition d, upperRightLegTip)
                 upperRightLegTip = otherEnd (backBonePosition d) (upperRightLegAngle d) upperLegLength
                 lowerRightLeg = (upperRightLegTip, upperRightLegTip, lowerRightLegTip)
                 lowerRightLegTip = otherEnd upperRightLegTip (lowerRightLegAngle d) lowerLegLength

