{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module Flowbox.Luna.FClasses.U'select4 where

class C''select4' a b | a -> b where
    select4'    :: a -> b
    select4'''M :: a -> IO b
