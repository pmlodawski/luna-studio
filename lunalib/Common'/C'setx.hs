{-# LANGUAGE FunctionalDependencies,
             FlexibleInstances #-}

module Common'.C'setx where

class C'setx a b | a->b where
    setx    :: a -> b
    setx''M :: a -> IO b
