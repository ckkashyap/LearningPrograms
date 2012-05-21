import Geometry
import JavaScript


test_points = map (\i -> (i,i+1,i-1)) [1..3] :: [Point3D]

test_triangle = (p1, p2, p3) where [p1, p2, p3] = test_points

test_scene = [TransformedTriangle test_triangle (0,0,0) (0,0,0)] :: Frame

test_animation = take 2 $ cycle [test_scene] :: Animation

output = putStr $ animation2JS test_animation

zCoordinate = 0




main = do
     putStrLn $ ""