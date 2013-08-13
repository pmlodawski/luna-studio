{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module Flowbox.Luna.FClasses.U'select10 where

class C''select10' a b | a -> b where
    select10'    :: a -> b
    select10'''M :: a -> IO b
