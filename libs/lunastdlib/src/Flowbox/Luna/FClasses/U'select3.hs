{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module Flowbox.Luna.FClasses.U'select3 where

class C''select3' a b | a -> b where
    select3'    :: a -> b
    select3'''M :: a -> IO b
