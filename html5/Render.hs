{-# LANGUAGE NamedFieldPuns #-}
module Render where
import AnimationData
import Config
import Control.Monad.State
import Trigonometry



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

                     p4  =  applyRotations (hx,hy+upperLegSize,hz) (lulx + sx) (luly+sy) (lulz+sz)
                     p5  =  applyRotations (hx,hy+upperLegSize+lowerLegSize,hz) (lulx + lk +sx) (luly+sy) (lulz+sz)
                     p6  =  applyRotations (hx,hy+upperLegSize,hz) (rulx+sx) (ruly+sy) (rulz+sz)
                     p7  =  applyRotations (hx,hy+upperLegSize+lowerLegSize,hz) (rulx + rk + sx) (ruly+sy) (rulz+sz)

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





convertSegmentsTo2D :: [Segment] -> [Segment2D]
convertSegmentsTo2D = foldr (\(p1,p2) rest -> (mapPointOnScreen p1, mapPointOnScreen p2):rest) []


segments2script :: [Segment2D] -> String
segments2script [] = ""
segments2script (((x1,y1),(x2,y2)):ss) =  "context.moveTo(" ++ (show x1) ++ ", " ++ (show y1) ++ ");\n" ++ "context.lineTo(" ++ (show x2) ++ ", " ++ (show y2) ++ ");\n" ++ segments2script ss




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


getJS :: MyState () -> String
getJS steps = "var actionList = [\n" ++ (foldr g [] (map f ps)) ++ "\n]\n"
      where
        f = segments2script . convertSegmentsTo2D . toSegments
        ps = getPositionList steps
        g scr [] = "function (context) {\n" ++ scr ++ "\n}\n"
        g scr rest = "function (context) {\n" ++ scr ++ "\n},\n" ++ rest
      
