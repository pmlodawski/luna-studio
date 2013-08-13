{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module Common'.C''x'setter where

class C''x'setter a b | a -> b where
    x'setter    :: a -> b
    x'setter''M :: a -> IO b
