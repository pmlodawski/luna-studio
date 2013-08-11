{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module Flowbox.Luna.FClasses.C''select7' where

class C''select7' a b | a -> b where
    select7'    :: a -> b
    select7'''M :: a -> IO b
