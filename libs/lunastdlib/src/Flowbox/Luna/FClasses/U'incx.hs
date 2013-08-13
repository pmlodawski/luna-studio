{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module Flowbox.Luna.FClasses.U'incx where

class C''incx' a b | a -> b where
    incx'    :: a -> b
    incx'''M :: a -> IO b
