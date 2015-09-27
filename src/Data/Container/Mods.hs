{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Container.Mods where

import Prologue hiding (Ixed)
import Data.TypeLevel.Bool
import Data.Typeable

data Safe      = Safe
data Unchecked = Unchecked
data Ixed      = Ixed
data Try       = Try


type family Mutable a :: Bool
type instance Mutable Ixed      = True
type instance Mutable Unchecked = False
type instance Mutable Try       = False

type family FilterMutable (lst :: [*]) :: [*] where
    FilterMutable '[] = '[]
    FilterMutable (l ': ls) = If (Mutable l) (l ': FilterMutable ls) (FilterMutable ls)

filterMutable :: Proxy a -> Proxy (FilterMutable a)
filterMutable _ = Proxy