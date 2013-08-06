{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module Common'.C''z'getter where

class C''z'getter a b | a -> b where
    z'getter    :: a -> b
    z'getter''M :: a -> IO b
