{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module Flowbox.Luna.FClasses.U'init where

class C''init' a b | a -> b where
    init'    :: a -> b
    init'''M :: a -> IO b
