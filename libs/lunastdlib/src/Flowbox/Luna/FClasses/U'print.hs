{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module Flowbox.Luna.FClasses.U'print where

class C''print' a b | a -> b where
    print'    :: a -> b
    print'''M :: a -> IO b
