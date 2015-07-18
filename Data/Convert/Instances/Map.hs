{- |
   Module     : Data.Convertible.Instances.Map
   Copyright  : Copyright (C) 2009-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Instances to convert between Map and association list.

Copyright (C) 2009-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE

-}

module Data.Convert.Instances.Map()
where

import Data.Convert.Base

import qualified Data.Map as Map

instance Ord k => Convertible [(k, a)] (Map.Map k a) where
    convert = Map.fromList
instance Convertible (Map.Map k a) [(k, a)] where
    convert = Map.toList
