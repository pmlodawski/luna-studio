{-# LANGUAGE CPP                  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Container.Auto where

import Prologue
import Data.Container.Class
import Data.Container.Resizable
import Data.Container.Reusable
import Data.Layer


#define Auto_DEF (Reusable idx (Resizable style a))

newtype Auto idx style a = Auto Auto_DEF deriving (Functor, Traversable, Foldable, Monoid, Default)
type    Auto'    style a = Auto (IndexOf (ContainerOf a)) style a

deriving instance (IsContainer a, FromList (ContainerOf a), Default style) => FromList (Auto idx style a)


type instance Item         (Auto idx style a) = Item        Auto_DEF
type instance IndexOf      (Auto idx style a) = IndexOf     Auto_DEF

type instance ContainerOf  (Auto idx style a) = ContainerOf Auto_DEF
instance      IsContainer  (Auto idx style a) where fromContainer = Auto . fromContainer
instance      HasContainer (Auto idx style a) where container     = layered . container

-- Wrappers & layers

type instance Unlayered  (Auto idx style a) = Auto_DEF
instance      Layered    (Auto idx style a)
instance      Wrapped    (Auto idx style a) where
    type      Unwrapped  (Auto idx style a) = Auto_DEF
    _Wrapped' = iso (\(Auto a) -> a) Auto




instance Show a => Show (Auto idx style a) where
    showsPrec d (Auto a) = showParen (d > app_prec) $
            showString "Auto " . showsPrec (succ app_prec) (view (layered . layered) a)
        where app_prec = 10

