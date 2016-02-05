---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE TypeFamilies          #-}


module Data.TypeLevel.FlatContainers where

import           Data.Typeable
import           GHC.TypeLits
import           Prelude       hiding (lookup)
import           Type.Bool

type family Contains set t where
  Contains ()    t = False
  Contains (t,x) t = True
  Contains (a,x) t = Contains x t


class Lookup s a where
  lookup :: s -> Maybe a

instance Lookup () a where
  lookup _ = Nothing

instance Lookup (a,xs) a where
  lookup (x,_) = Just x

instance Lookup xs a => Lookup (x,xs) a where
  lookup (_,xs) = lookup xs


class Modify s a where
  modify :: (a -> a) -> s -> s

instance Modify () a where
  modify _ = id

instance Modify xs a => Modify (x,xs) a where
  modify f (x,xs) = (x, modify f xs)

instance Modify (a,xs) a where
  modify f (a,xs) = (f a, xs)

