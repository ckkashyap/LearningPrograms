{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad.State
import AnimationData
import Config
import Render

          

moveShoulderRight :: MyState ()
moveShoulderRight = do
     (p@(Position {shoulder = (x,y,z)}):ps) <- get
     let n = p { shoulder = (x,y+1,z)}
     put (n:p:ps)
     return ()

moveShoulderLeft :: MyState ()
moveShoulderLeft = do
     (p@(Position {shoulder = (x,y,z)}):ps) <- get
     let n = p { shoulder = (x,y-1,z)}
     put (n:p:ps)
     return ()



moveRightHand :: MyState ()
moveRightHand = do
     (p@(Position {rightUpperArm = (x,y,z), rightElbow = re}):ps) <- get
     let n = p { rightUpperArm = (x+1,y+1,z), rightElbow = re + 1 }
     put (n:p:ps)
     return ()

moveRightHand' :: MyState ()
moveRightHand' = do
     (p@(Position {rightUpperArm = (x,y,z), rightElbow = re}):ps) <- get
     let n = p { rightUpperArm = (x-1,y-1,z), rightElbow = re -1 }
     put (n:p:ps)
     return ()

moveLeftHand :: MyState ()
moveLeftHand = do
     (p@(Position {leftUpperArm = (x,y,z), leftElbow = le}):ps) <- get
     let n = p { leftUpperArm = (x+1,y-1,z), leftElbow = le }
     put (n:p:ps)
     return ()

moveLeftHand' :: MyState ()
moveLeftHand' = do
     (p@(Position {leftUpperArm = (x,y,z), leftElbow = le}):ps) <- get
     let n = p { leftUpperArm = (x-1,y+1,z), leftElbow = le }
     put (n:p:ps)
     return ()





dance' :: Int -> MyState () -> MyState ()
dance' 0 _ = return ()
dance' n a = do
       a
       dance' (n-1) a





dance :: MyState ()
dance = do
      dance' 10 (moveLeftHand >>  moveRightHand >> moveShoulderLeft)
      dance' 20 (moveLeftHand' >> moveRightHand' >> moveShoulderRight)
      dance' 10 (moveLeftHand >>  moveRightHand >> moveShoulderLeft)


getJS :: MyState () -> String
getJS steps = "var actionList = [\n" ++ (foldr g [] (map f ps)) ++ "\n]\n"
      where
        f = segments2script . convertSegmentsTo2D . toSegments
        ps = getPositionList steps
        g scr [] = "function (context) {\n" ++ scr ++ "\n}\n"
        g scr rest = "function (context) {\n" ++ scr ++ "\n},\n" ++ rest
      



main = do
--  putStrLn (getJS dance)
  writeFile "actionList.js" (getJS dance)
  return ()