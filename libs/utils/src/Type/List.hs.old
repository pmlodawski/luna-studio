{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Type.List where

import GHC.TypeLits

type family IndexOf (a :: k) (as :: [k]) :: Nat where
    IndexOf a (a ': as) = 0
    IndexOf a (b ': as) = IndexOf a as + 1

type family DeleteIdx (num::Nat) (as :: [k]) :: [k] where
    DeleteIdx 0 (a ': as) = as
    DeleteIdx n (a ': as) = a ': DeleteIdx (n-1) as