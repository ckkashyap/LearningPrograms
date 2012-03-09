{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module TmpMod1 where

import Language.Haskell.TH

fstN :: Int -> Q Exp
fstN n = do
     x <- newName "x"
     return $ LamE [TupP $
            VarP x : replicate (n-1) WildP] (VarE x)
