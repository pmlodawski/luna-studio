{-# LANGUAGE FunctionalDependencies,
             FlexibleInstances #-}

module Common'.C'getx where

class C'getx a b | a->b where
    getx    :: a -> b
    getx''M :: a -> IO b
