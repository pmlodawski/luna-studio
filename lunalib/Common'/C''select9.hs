{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module Common'.C''select9 where

class C''select9 a b | a -> b where
    select9    :: a -> b
    select9''M :: a -> IO b
