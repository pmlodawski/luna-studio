{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module Flowbox.Luna.FClasses.C''y'setter' where

class C''y'setter' a b | a -> b where
    y'setter'    :: a -> b
    y'setter'''M :: a -> IO b
