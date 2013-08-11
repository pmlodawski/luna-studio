{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module Flowbox.Luna.FClasses.C''x'getter' where

class C''x'getter' a b | a -> b where
    x'getter'    :: a -> b
    x'getter'''M :: a -> IO b
