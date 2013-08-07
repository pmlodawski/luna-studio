{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module Common'.C''select7 where

class C''select7 a b | a -> b where
    select7    :: a -> b
    select7''M :: a -> IO b
