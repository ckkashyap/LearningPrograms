module Dancer where
import Geometry

type Coordinates3D = (Double, Double, Double)

data Dancer = Dancer {
     neckAngleX          :: Double,
     neckAngleY          :: Double,
     neckAngleZ          :: Double,
     backBoneAngleX      :: Double,
     backBoneAngleY      :: Double,
     backBoneAngleZ      :: Double,
     backBonePositionX   :: Double,
     backBonePositionY   :: Double,
     backBonePositionZ   :: Double,
     upperLeftArmAngleX  :: Double,
     upperLeftArmAngleY  :: Double,
     upperLeftArmAngleZ  :: Double,
     upperRightArmAngleX :: Double,
     upperRightArmAngleY :: Double,
     upperRightArmAngleZ :: Double,
     lowerLeftArmAngleX  :: Double,
     lowerLeftArmAngleY  :: Double,
     lowerLeftArmAngleZ  :: Double,
     lowerRightArmAngleX :: Double,
     lowerRightArmAngleY :: Double,
     lowerRightArmAngleZ :: Double,
     upperLeftLegAngleX  :: Double,
     upperLeftLegAngleY  :: Double,
     upperLeftLegAngleZ  :: Double,
     lowerLeftLegAngleX  :: Double,
     lowerLeftLegAngleY  :: Double,
     lowerLeftLegAngleZ  :: Double,
     upperRightLegAngleX :: Double,
     upperRightLegAngleY :: Double,
     upperRightLegAngleZ :: Double,
     lowerRightLegAngleX :: Double,
     lowerRightLegAngleY :: Double,
     lowerRightLegAngleZ :: Double
     } deriving Show

neckAngle          :: Dancer -> (Double, Double, Double)
neckAngle d = (neckAngleX d, neckAngleY d, neckAngleZ d)

backBoneAngle      :: Dancer -> (Double, Double, Double)
backBoneAngle d = (backBoneAngleX d, backBoneAngleY d, backBoneAngleZ d)

backBonePosition   :: Dancer -> (Double, Double, Double)
backBonePosition d = (backBonePositionX d, backBonePositionY d, backBonePositionZ d)

upperLeftArmAngle  :: Dancer -> (Double, Double, Double)
upperLeftArmAngle d = (upperLeftArmAngleX d, upperLeftArmAngleY d, upperLeftArmAngleZ d)

upperRightArmAngle :: Dancer -> (Double, Double, Double)
upperRightArmAngle d = (upperRightArmAngleX d, upperRightArmAngleY d, upperRightArmAngleZ d)

lowerLeftArmAngle  :: Dancer -> (Double, Double, Double)
lowerLeftArmAngle d = (lowerLeftArmAngleX d, lowerLeftArmAngleY d, lowerLeftArmAngleZ d)

lowerRightArmAngle :: Dancer -> (Double, Double, Double)
lowerRightArmAngle d = (lowerRightArmAngleX d, lowerRightArmAngleY d, lowerRightArmAngleZ d)

upperLeftLegAngle  :: Dancer -> (Double, Double, Double)
upperLeftLegAngle d = (upperLeftLegAngleX d, upperLeftLegAngleY d, upperLeftLegAngleZ d)

lowerLeftLegAngle  :: Dancer -> (Double, Double, Double)
lowerLeftLegAngle d = (lowerLeftLegAngleX d, lowerLeftLegAngleY d, lowerLeftLegAngleZ d)

upperRightLegAngle :: Dancer -> (Double, Double, Double)
upperRightLegAngle d = (upperRightLegAngleX d, upperRightLegAngleY d, upperRightLegAngleZ d)

lowerRightLegAngle :: Dancer -> (Double, Double, Double)
lowerRightLegAngle d = (lowerRightLegAngleX d, lowerRightLegAngleY d, lowerRightLegAngleZ d)

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

