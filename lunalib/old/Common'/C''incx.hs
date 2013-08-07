{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module Common'.C''incx where

class C''incx a b | a -> b where
    incx    :: a -> b
    incx''M :: a -> IO b
