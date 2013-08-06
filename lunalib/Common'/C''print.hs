{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module Common'.C''print where

class C''print a b | a -> b where
    print    :: a -> b
    print''M :: a -> IO b
