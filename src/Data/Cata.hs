{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE RankNTypes           #-}

-- UndecidableInstances are used only for Show instances
{-# LANGUAGE UndecidableInstances #-}

module Data.Cata where

import Flowbox.Prelude
import Control.Lens
import Control.Monad (join)
import Data.Repr

-- === Catamorphisms ===

newtype Mu  f   = Mu  ( f    (Mu f)     )
newtype MuH h f = MuH ( f (h (MuH h f)) )

type family MuData (mu :: (* -> *) -> *) a

type MendlerAlgebra f c = forall a. (a -> c) -> f a -> c
type Algebra        f a = f a -> a

type MuGenLayout    mu f a = f (MuData mu a)
type MuLayout       mu f   = MuGenLayout mu f (mu f)

type MuMap          mu f   c = (mu f -> c)   -> MuLayout mu f -> MuGenLayout mu f c
type MuMapM         mu f m c = (mu f -> m c) -> MuLayout mu f -> m (MuGenLayout mu f c)

class IsMu mu where
    toMu   :: MuLayout mu f -> mu f
    fromMu :: mu f          -> MuLayout mu f

class MuFunctor mu where
    mumap :: Functor f => MuMap mu f c

class MuTraversable mu where
    mumapM :: (Monad m, Traversable f) => MuMapM mu f m c

--- utils ---

mu :: IsMu mu => Lens' (mu f) (MuLayout mu f)
mu = lens fromMu (const toMu)

cata  :: (IsMu mu, MuFunctor     mu, Functor     f                    ) => (MuGenLayout mu f c ->   c) -> mu f ->   c
cataM :: (IsMu mu, MuTraversable mu, Traversable f, Monad m, Functor m) => (MuGenLayout mu f c -> m c) -> mu f -> m c
cata  = cataOver  mumap
cataM = cataOverM mumapM

cataOver  :: (IsMu mu                    ) => MuMap  mu f   c -> (MuGenLayout mu f c ->   c) -> mu f ->   c
cataOverM :: (IsMu mu, Monad m, Functor m) => MuMapM mu f m c -> (MuGenLayout mu f c -> m c) -> mu f -> m c
cataOver  g f =             f . g (cataOver  g f) . view mu
cataOverM g f = join . fmap f . g (cataOverM g f) . view mu

mendlerCata :: MendlerAlgebra f c -> Mu f -> c
mendlerCata phi = phi (mendlerCata phi) . (view mu)

-- | The same as Mendler-style catamorphism:
-- | muCata phi = mendlerCata (\f -> phi . fmap f)
muCata :: Functor f => Algebra f c -> Mu f -> c
muCata = cata

muHCata :: (Functor h, Functor f) => (f (h c) -> c) -> MuH h f -> c
muHCata = cata

--- instances ---

-- MuData
type instance MuData Mu      a = a
type instance MuData (MuH h) a = h a

-- IsMu
instance IsMu Mu where
    toMu             = Mu
    fromMu (Mu a)    = a

instance IsMu (MuH h) where
    toMu                  = MuH
    fromMu (MuH a)        = a

-- MuFunctor
instance              MuFunctor Mu      where mumap = fmap
instance Functor h => MuFunctor (MuH h) where mumap = fmap . fmap

-- MuTraversable
instance                  MuTraversable Mu      where mumapM = mapM
instance Traversable h => MuTraversable (MuH h) where mumapM = mapM . mapM

-- Repr
instance Repr (MuLayout Mu      f) => Repr (Mu f)    where repr = repr . view mu
instance Repr (MuLayout (MuH h) f) => Repr (MuH h f) where repr = repr . view mu

-- Show
deriving instance Show (MuLayout Mu      f) => Show (Mu f)
deriving instance Show (MuLayout (MuH h) f) => Show (MuH h f)
