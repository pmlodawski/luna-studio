{-# LANGUAGE CPP                  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Container.Auto where

import Prologue
import Data.Container.Class
import Data.Container.Resizable
import Data.Container.Reusable
import Data.Layer


#define AUTO (Reusable idx (Resizable style a))

newtype Auto idx style a = Auto AUTO deriving (Functor, Traversable, Foldable, Monoid, Default)
type    Auto'    style a = Auto (IndexOf (ContainerOf a)) style a

deriving instance (IsContainer a, FromList (ContainerOf a), Default style) => FromList (Auto idx style a)


type instance Item         (Auto idx style a) = Item    AUTO
type instance IndexOf      (Auto idx style a) = IndexOf AUTO

type instance            DataStoreOf     (Auto idx style a) = DataStoreOf AUTO
type instance            ContainerOf     (Auto idx style a) = ContainerOf AUTO
instance      Monad m => IsContainerM  m (Auto idx style a) where fromContainerM = fmap Auto . fromContainerM
instance      Monad m => HasContainerM m (Auto idx style a) where viewContainerM = viewContainerM . view layered
                                                                  setContainerM  = layered . setContainerM

-- Wrappers & layers

type instance       Unlayered  (Auto idx style a) = AUTO
instance            Layered    (Auto idx style a)
instance Monad m => LayeredM m (Auto idx style a)
instance            Wrapped    (Auto idx style a) where
    type            Unwrapped  (Auto idx style a) = AUTO
    _Wrapped' = iso (\(Auto a) -> a) Auto




instance Show a => Show (Auto idx style a) where
    showsPrec d (Auto a) = showParen (d > app_prec) $
            showString "Auto " . showsPrec (succ app_prec) (view (layered . layered) a)
        where app_prec = 10

