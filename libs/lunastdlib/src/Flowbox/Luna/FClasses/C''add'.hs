{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module Flowbox.Luna.FClasses.C''add' where

class C''add' a b | a -> b where
    add'    :: a -> b
    add'''M :: a -> IO b
