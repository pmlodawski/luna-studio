---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------


{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Maps  where

import Prelude hiding (lookup)
import qualified Data.Map    as Map
import qualified Data.IntMap as IntMap
import Data.Monoid


----------------------------------------------------------------------
-- Tpye classes
----------------------------------------------------------------------

class Map m k where
    null         :: m a -> Bool
    size         :: m a -> Int
    member       :: k -> m a -> Bool
    lookup       :: k -> m a -> Maybe a
    insert       :: k -> a -> m a -> m a
    insertWith   :: (a -> a -> a) -> k -> a -> m a -> m a
    foldrWithKey :: (k -> a -> b -> b) -> b -> m a -> b
    delete       :: k -> m a -> m a


----------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------

notMember k m = not $ member k m

findWithDefault def k m = case lookup k m of
      Nothing -> def
      Just x  -> x

singleton k a = insert k a empty

empty = mempty

keys = foldrWithKey (\k _ ks -> k : ks) []

elems = foldr (:) []

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

-- Data.Map

instance Ord k => Map (Map.Map k) k where
    null         = Map.null
    size         = Map.size
    member       = Map.member
    lookup       = Map.lookup
    insert       = Map.insert
    insertWith   = Map.insertWith
    foldrWithKey = Map.foldrWithKey
    delete       = Map.delete

-- Data.IntMap

instance Map IntMap.IntMap IntMap.Key where
    null         = IntMap.null
    size         = IntMap.size
    member       = IntMap.member
    lookup       = IntMap.lookup
    insert       = IntMap.insert
    insertWith   = IntMap.insertWith
    foldrWithKey = IntMap.foldrWithKey
    delete       = IntMap.delete




