{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module Common'.C''y'getter where

class C''y'getter a b | a -> b where
    y'getter    :: a -> b
    y'getter''M :: a -> IO b
