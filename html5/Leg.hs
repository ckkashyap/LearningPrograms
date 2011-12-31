module Leg where
import Control.Monad.State
import AnimationData
import Config
import Render

spreadLeg :: MyState ()
spreadLeg = do
          (p@Position {hip=(hx,hy,hz)}:ps) <- get
          let n=p {leftUpperLeg=(0,0,-30),rightUpperLeg=(0,0,30), hip=(hx,hy+10,hz)}
          put (n:p:ps)
          return ()




