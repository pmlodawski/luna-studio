{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module Common'.C''x'getter where

class C''x'getter a b | a -> b where
    x'getter    :: a -> b
    x'getter''M :: a -> IO b
