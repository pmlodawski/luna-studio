{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module Flowbox.Luna.FClasses.U'select2 where

class C''select2' a b | a -> b where
    select2'    :: a -> b
    select2'''M :: a -> IO b
