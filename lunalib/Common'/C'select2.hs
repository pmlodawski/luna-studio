{-# LANGUAGE FunctionalDependencies,
             FlexibleInstances #-}

module Common'.C'select2 where

class C'select2 a b | a->b where
    select2    :: a -> b
    select2''M :: a -> IO b
