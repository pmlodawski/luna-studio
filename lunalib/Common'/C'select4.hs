{-# LANGUAGE FunctionalDependencies,
             FlexibleInstances #-}

module Common'.C'select4 where

class C'select4 a b | a->b where
    select4    :: a -> b
    select4''M :: a -> IO b
