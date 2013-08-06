{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module Common'.C''z'setter where

class C''z'setter a b | a -> b where
    z'setter    :: a -> b
    z'setter''M :: a -> IO b
