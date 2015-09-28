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


newtype Auto m a = Auto (Reusable (Resizable Exponential m) a) deriving (Functor, Traversable, Foldable)

type instance DataStoreOf (Auto m a) = DataStoreOf (Reusable (Resizable Exponential m) a)
type instance ContainerOf (Auto m a) = ContainerOf (Reusable (Resizable Exponential m) a)

instance IsContainer  (Auto m a) where fromContainer = Auto . fromContainer
instance HasContainer (Auto m a) where container     = (lens (\(Auto a) -> a) (const Auto)) . container

instance Show (m a) => Show (Auto m a) where
    showsPrec d (Auto a) = showParen (d > app_prec) $
            showString "Auto " . showsPrec (app_prec+1) (view (parametrized . wrapped . parametrized . wrapped) a)
         where app_prec = 10

instance Default (m a) => Default (Auto m a) where def = Auto def