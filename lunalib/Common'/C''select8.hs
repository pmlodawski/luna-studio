{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module Common'.C''select8 where

class C''select8 a b | a -> b where
    select8    :: a -> b
    select8''M :: a -> IO b
