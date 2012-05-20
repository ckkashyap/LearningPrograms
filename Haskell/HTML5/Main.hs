import Geometry
import JavaScript


test_points = map (\i -> (i,i+1,i-1)) [1..3] :: [Point3D]

test_triangle = (p1, p2, p3) where [p1, p2, p3] = test_points

test_scene = [test_triangle] :: Scene

test_animation = take 2 $ cycle [test_scene] :: Animation

output = putStr $ animation2JS test_animation

zCoordinate = 0

cube :: Scene
cube = [
     
     ((-50,-50, zCoordinate-20), (50, -50, zCoordinate-20), (0, 0, zCoordinate-20))

     ] 



main = do
     putStrLn $ ""