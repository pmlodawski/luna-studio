{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module Flowbox.Luna.FClasses.U'select8 where

class C''select8' a b | a -> b where
    select8'    :: a -> b
    select8'''M :: a -> IO b
