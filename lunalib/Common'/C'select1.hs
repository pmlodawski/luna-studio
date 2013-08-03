{-# LANGUAGE FunctionalDependencies,
             FlexibleInstances #-}

module Common'.C'select1 where

class C'select1 a b | a->b where
    select1    :: a -> b
    select1''M :: a -> IO b
