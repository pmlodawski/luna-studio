{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module Common'.C''select0 where

class C''select0 a b | a -> b where
    select0    :: a -> b
    select0''M :: a -> IO b
