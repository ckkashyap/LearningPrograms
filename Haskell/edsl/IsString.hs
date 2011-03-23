{-# LANGUAGE OverloadedStrings #-}

module Main where

import GHC.Exts( IsString(..) )

newtype MyString = MyString String deriving (Eq, Show)


instance IsString MyString where
    fromString = MyString

greet :: MyString -> MyString
greet "hello" = "world"
greet other = other

main = do
    print $ greet "hello"
    print $ greet "fool"

