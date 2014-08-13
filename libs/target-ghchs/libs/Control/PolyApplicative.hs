{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}


module Control.PolyApplicative where

class PolyApplicative m1 m2 m3 | m1 m2 -> m3 where
    (<<*>>) :: m1 (a -> b) -> m2 a -> m3 b



