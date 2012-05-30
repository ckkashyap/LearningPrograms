module Dancer where
import Geometry

type Coordinates3D = (Double, Double, Double)

data Dancer = Dancer {
     
     neckAngle :: Coordinates3D,
     
     backBoneAngle :: Coordinates3D,

     backBonePosition :: Coordinates3D,
 
     upperLeftArmAngle :: Coordinates3D,

     upperRightArmAngle :: Coordinates3D,

     lowerLeftArmAngle :: Coordinates3D,

     lowerRightArmAngle :: Coordinates3D,

     upperLeftLegAngle :: Coordinates3D,

     lowerLeftLegAngle :: Coordinates3D,

     upperRightLegAngle :: Coordinates3D,

     lowerRightLegAngle :: Coordinates3D
     } deriving Show


otherEnd :: Point3D -> Coordinates3D -> Double -> Point3D
otherEnd (x, y, z) (xa, ya, za) d = (x', y', z') where
         x' = x + (d * (cos zr) * (sin yr))
         y' = y - (d * (sin zr))
         z' = z + (d * (cos zr) * (cos yr))
         zr = pi *  za / 180.0
         yr = pi *  ya / 180.0
         
                

backBoneLength = 50
upperArmLength = 20
lowerArmLength = 15
neckLength = 5

dancer2triangles :: Dancer -> [Triangle3D]
dancer2triangles d = [ neck, backBone, upperLeftArm, upperRightArm, lowerLeftArm, lowerRightArm]  where
--dancer2triangles d = [ neck, backBone, upperLeftArm, upperRightArm, lowerLeftLeg, lowerRightLeg ]  where
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

                 


