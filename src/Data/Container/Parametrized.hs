{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds    #-}

module Data.Container.Parametrized where


import Prologue
import Data.Container.Class
import Data.Container.Poly

newtype Parametrized t (m :: * -> *) a = Parametrized { _parametrized :: t (m a) } deriving (Show, Functor, Traversable, Foldable)

makeLenses ''Parametrized


type instance ModsOf (cls :: k) (Parametrized t m a) = ModsOf cls (t (m a))

type instance DataStoreOf (Parametrized t m a) = DataStoreOf (t (m a))
type instance ContainerOf (Parametrized t m a) = ContainerOf (t (m a))

instance IsContainer  (t (m a)) => IsContainer  (Parametrized t m a) where fromContainer = Parametrized . fromContainer
instance HasContainer (t (m a)) => HasContainer (Parametrized t m a) where container     = lens (\(Parametrized c) -> c) (const Parametrized) . container

type instance ElementOf  (Parametrized t m a) = ElementOf  (t (m a))
type instance IndexOf el (Parametrized t m a) = IndexOf el (t (m a))


type instance HomoIndexOf (Parametrized t m) = HomoIndexOf m