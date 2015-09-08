{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Data.Pool ( module Data.Pool
                 , module X
                 ) where

import Prelude
import Control.Lens
import GHC.Exts
import Data.Containers as X
import Data.Default
import Data.Monoid


data Pool a = Pool { _free :: [a] }

makeClassy ''Pool

type instance ElementOf (Pool a) = a

instance Requestable (Pool a) where request (Pool (a:as)) = (a, Pool as)
instance Releasable  (Pool a) where release a (Pool lst)  = (Pool $ a : lst)

instance IsList (Pool a) where
    type Item (Pool a) = a
    fromList        = Pool
    toList (Pool l) = l

instance Default (Pool a) where
    def = Pool def

instance Monoid (Pool a) where
    mempty                    = Pool mempty
    mappend (Pool a) (Pool b) = Pool $ a <> b