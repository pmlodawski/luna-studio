{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module Data.Container.Auto where

import Prologue
import Data.Container.Class
import Data.Container.Poly
import Data.Container.Resizable
import Data.Container.Reusable
import Data.Container.Parametrized
import Data.Layer
import Data.Container.Weak


newtype Auto     m a =     Auto       (Reusable (Resizable Exponential m)  a) deriving (Functor, Traversable, Foldable, FromList, Monoid, Default)
newtype WeakAuto m a = WeakAuto (Weak (Reusable (Resizable Exponential m)) a) deriving (FromList)


type instance Item        (Auto m a) = Item        (Unwrapped (Auto m a))
type instance DataStoreOf (Auto m a) = DataStoreOf (Unwrapped (Auto m a))
type instance ContainerOf (Auto m a) = ContainerOf (Unwrapped (Auto m a))
type instance ElementOf   (Auto m a) = a
type instance IndexOf   a (Auto m a) = HomoIndexOf m
type instance HomoIndexOf (Auto m)   = HomoIndexOf m


type instance Item        (WeakAuto m a) = Item        (Unwrapped (WeakAuto m a))
type instance DataStoreOf (WeakAuto m a) = DataStoreOf (Unwrapped (WeakAuto m a))
type instance ContainerOf (WeakAuto m a) = ContainerOf (Unwrapped (WeakAuto m a))
type instance ElementOf   (WeakAuto m a) = a
type instance IndexOf   a (WeakAuto m a) = HomoIndexOf m
type instance HomoIndexOf (WeakAuto m)   = HomoIndexOf m


-- Wrappers & layers

type instance Unlayered  (Auto m a) = Unwrapped (Auto m a)
instance      Layered    (Auto m a)
instance      Wrapped    (Auto m a) where
	type      Unwrapped  (Auto m a) = Reusable (Resizable Exponential m) a
	_Wrapped' = iso (\(Auto a) -> a) Auto

type instance Unlayered  (WeakAuto m a) = Unwrapped (WeakAuto m a)
instance      Layered    (WeakAuto m a)
instance      Wrapped    (WeakAuto m a) where
	type      Unwrapped  (WeakAuto m a) = Weak (Reusable (Resizable Exponential m)) a
	_Wrapped' = iso (\(WeakAuto a) -> a) WeakAuto



instance IsContainer  (Auto m a) where fromContainer = Auto . fromContainer
instance HasContainer (Auto m a) where container     = layered . container

instance IsContainer  (WeakAuto m a) where fromContainer = WeakAuto . fromContainer
instance HasContainer (WeakAuto m a) where container     = layered . container


instance Show (m a) => Show (Auto m a) where
    showsPrec d (Auto a) = showParen (d > app_prec) $
            showString "Auto " . showsPrec (app_prec+1) (view (parametrized . layered . parametrized . layered) a)
         where app_prec = 10

