{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.PolyMonad where

class PolyMonad m1 m2 m3 | m1 m2 -> m3 where
    (>>=~) :: m1 a -> (a -> m2 b) -> m3 b

polyBind :: PolyMonad m1 m2 m3 => m1 a -> (a -> m2 b) -> m3 b
polyBind = (>>=~)

polyJoin :: PolyMonad m1 m2 m3 => m1 (m2 a) -> m3 a
polyJoin = (>>=~ id)

