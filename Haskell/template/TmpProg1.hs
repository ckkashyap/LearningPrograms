{-# LANGUAGE TemplateHaskell #-}

import TmpMod1



main = do
     print $ $(fstN 3) ("hello world",2,3)