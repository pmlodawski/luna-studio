{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module Flowbox.Luna.FClasses.U'select1 where

class C''select1' a b | a -> b where
    select1'    :: a -> b
    select1'''M :: a -> IO b
