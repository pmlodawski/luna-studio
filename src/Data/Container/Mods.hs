{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds            #-}

-- TODO[WD]: Rename to Data.Container.Opts
module Data.Container.Mods where

import Prologue hiding (Ixed)
import Type.Bool
import Data.Typeable

-- === Opts===




-- === Mods ===

data Ixed      = Ixed



-- === Parameters ===

data Safe      = Safe
data Unchecked = Unchecked
data Unsafe    = Unsafe

-- Formatters

data Try       = Try
data Raw       = Raw








--- DELETE vvvvvvvvv
type family Mutable a :: Bool
type instance Mutable Ixed      = True
type instance Mutable Unsafe    = False
type instance Mutable Unchecked = False
type instance Mutable Try       = False

type family FilterMutable (lst :: [*]) :: [*] where
    FilterMutable '[] = '[]
    FilterMutable (l ': ls) = If (Mutable l) (l ': FilterMutable ls) (FilterMutable ls)

filterMutable :: Proxy a -> Proxy (FilterMutable a)
filterMutable _ = Proxy