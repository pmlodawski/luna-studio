{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Map.IntConvertibleSet (
    IntConvertibleSet,
    toList, fromList,
    empty, size, --null,
    singleton, --insert, insertWith, insertWithKey,
    --lookup, findWithDefault,
    --delete, member, notMember,
    union, unions, difference, (\\), intersection --map, foldrWithKey
  ) where


import            Flowbox.Prelude     (Maybe(..), Int, Bool, ($), (.), fmap)

import            Data.Function       (on)
import Data.Monoid

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet


import            Data.Proxy

import            Data.Packable




instance (Unpack k Int) => Monoid (IntConvertibleSet k) where
    mempty  = empty
    mappend = union
    mconcat = unions

data IntConvertibleSet k where IntConvertibleSet :: (Unpack k Int) => Proxy k -> IntSet -> IntConvertibleSet k


toIntSet :: IntConvertibleSet k -> IntSet
toIntSet (IntConvertibleSet _ mp) = mp




toList :: (Packable k Int) => IntConvertibleSet k -> [k]
toList = fmap pack . IntSet.toList . toIntSet


fromList :: (Unpack k Int) => [k] -> IntConvertibleSet k
fromList = IntConvertibleSet (Proxy::Proxy k) . IntSet.fromList . fmap unpack


singleton :: (Unpack k Int) => k -> IntConvertibleSet k
singleton = IntConvertibleSet (Proxy::Proxy k) . IntSet.singleton . unpack


empty :: (Unpack k Int) => IntConvertibleSet k
empty = IntConvertibleSet (Proxy::Proxy k) $ IntSet.empty


--lookup :: (Unpack k Int) => k -> IntConvertibleSet k -> Maybe v
--lookup k = IntSet.lookup (unpack k) . toIntSet


union :: (Unpack k Int) => IntConvertibleSet k -> IntConvertibleSet k -> IntConvertibleSet k
union mp1 mp2 = IntConvertibleSet (Proxy::Proxy k) $ (IntSet.union `on` toIntSet) mp1 mp2

unions :: (Unpack k Int) => [IntConvertibleSet k] -> IntConvertibleSet k
unions = IntConvertibleSet (Proxy::Proxy k) . IntSet.unions . fmap toIntSet


intersection :: (Unpack k Int) => IntConvertibleSet k -> IntConvertibleSet k -> IntConvertibleSet k
intersection mp1 mp2 = IntConvertibleSet (Proxy::Proxy k) $ (IntSet.intersection `on` toIntSet) mp1 mp2


(\\), difference :: (Unpack k Int) => IntConvertibleSet k -> IntConvertibleSet k -> IntConvertibleSet k
difference a b = IntConvertibleSet (Proxy::Proxy k) $ (IntSet.difference `on` toIntSet) a b
(\\) = difference

--map :: (Unpack k Int) => (a -> b) -> IntConvertibleSet k a -> IntConvertibleSet k b
--map f = IntConvertibleSet (Proxy::Proxy k) . IntSet.map f . toIntSet


--insertWithKey :: (Packable k Int) => (k -> v -> v -> v) -> k -> v -> IntConvertibleSet k -> IntConvertibleSet k
--insertWithKey f k a = IntConvertibleSet (Proxy::Proxy k) . IntSet.insertWithKey (f . pack) (unpack k) a . toIntSet


--foldrWithKey :: (Pack Int k) => (k -> v -> a -> a) -> a -> IntConvertibleSet k -> a
--foldrWithKey f a = IntSet.foldrWithKey (f . pack) a . toIntSet


--delete :: (Unpack k Int) => k -> IntConvertibleSet k -> IntConvertibleSet k
--delete k = IntConvertibleSet (Proxy::Proxy k) . IntSet.delete (unpack k) . toIntSet


size :: IntConvertibleSet k -> Int
size = IntSet.size . toIntSet


--null :: IntConvertibleSet k -> Bool
--null = IntSet.null . toIntSet


--insert :: (Unpack k Int) => k -> v -> IntConvertibleSet k -> IntConvertibleSet k
--insert k v = IntConvertibleSet (Proxy::Proxy k) . IntSet.insert (unpack k) v . toIntSet


--insertWith :: (Unpack k Int) => (v -> v -> v) -> k -> v -> IntConvertibleSet k -> IntConvertibleSet k
--insertWith f k v = IntConvertibleSet (Proxy::Proxy k) . IntSet.insertWith f (unpack k) v . toIntSet


--member :: (Unpack k Int) => k -> IntConvertibleSet k -> Bool
--member k = IntSet.member (unpack k) . toIntSet


--notMember :: (Unpack k Int) => k -> IntConvertibleSet k -> Bool
--notMember k = IntSet.notMember (unpack k) . toIntSet


--findWithDefault :: (Unpack k Int) => v -> k -> IntConvertibleSet k -> v
--findWithDefault d k = IntSet.findWithDefault d (unpack k) . toIntSet

