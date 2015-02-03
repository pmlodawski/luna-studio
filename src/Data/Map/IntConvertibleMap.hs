{-# LANGUAGE GADTs #-}

module Data.Map.IntConvertibleMap (
    IntConvertibleMap,
    toList, fromList, singleton, empty, lookup, union, map
  ) where


import            Flowbox.Prelude     hiding (toList , fromList , empty , lookup, map)

import            Data.Function       (on)

import            Data.IntMap.Strict  (IntMap)
import qualified  Data.IntMap.Strict  as IntMap

import            Data.Proxy

import            Data.Packable



data IntConvertibleMap k v where IntConvertibleMap :: (Unpack k Int) => Proxy k -> IntMap v -> IntConvertibleMap k v


toIntMap :: IntConvertibleMap k v -> IntMap v
toIntMap (IntConvertibleMap _ mp) = mp



toList :: (Packable k Int) => IntConvertibleMap k v -> [(k, v)]
toList = fmap packKey . IntMap.toList . toIntMap
  where packKey (k,v) = (pack k, v)

fromList :: (Unpack k Int) => [(k, v)] -> IntConvertibleMap k v
fromList = IntConvertibleMap (Proxy::Proxy k) . IntMap.fromList . fmap unpackKey
  where unpackKey (k, v) = (unpack k, v)

singleton :: (Unpack k Int) => k -> v -> IntConvertibleMap k v
singleton k = IntConvertibleMap (Proxy::Proxy k) . IntMap.singleton (unpack k)

empty :: (Unpack k Int) => IntConvertibleMap k v
empty = IntConvertibleMap (Proxy::Proxy k) $ IntMap.empty

lookup :: (Unpack k Int) => k -> IntConvertibleMap k v -> Maybe v
lookup k = IntMap.lookup (unpack k) . toIntMap

union :: (Unpack k Int) => IntConvertibleMap k v -> IntConvertibleMap k v -> IntConvertibleMap k v
union mp1 mp2 = IntConvertibleMap (Proxy::Proxy k) $ (IntMap.union `on` toIntMap) mp1 mp2

map :: (Unpack k Int) => (a -> b) -> IntConvertibleMap k a -> IntConvertibleMap k b
map f = IntConvertibleMap (Proxy::Proxy k) . IntMap.map f . toIntMap

