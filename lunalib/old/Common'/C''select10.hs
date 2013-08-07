{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module Common'.C''select10 where

class C''select10 a b | a -> b where
    select10    :: a -> b
    select10''M :: a -> IO b
