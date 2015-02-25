{-# LANGUAGE UndecidableInstances #-}

module Data.IntConvertibleSet.Strict (
    IntConvertibleSet,
    (\\),
    null, size,
    member, notMember,
    --lookupLT, lookupGT, lookupLE, lookupGE,
    isSubsetOf, isProperSubsetOf,
    empty, singleton,
    insert, delete,
    union, unions,
    difference, intersection,
    filter, partition, split,
    --splitMember, splitRoot,
    map,
    foldr, foldl, foldr', foldl',
    --fold,
    --findMin, findMax, deleteMin, deleteMax, deleteFindMin, deleteFindMax, maxView, minView,
    elems,
    toList, fromList,
    toAscList, toDescList, fromAscList, fromDistinctAscList
    --showTree, showTreeWith,
  ) where


import            Flowbox.Prelude   (uncurry, Int, Bool, ($), (.), fmap)
import            Data.Function     (on)
import            Data.Monoid

import            Data.IntSet       (IntSet)
import qualified  Data.IntSet       as IntSet

import            Data.Proxy
import            Data.Packable


instance (Packable k Int) => Monoid (IntConvertibleSet k) where
    mempty  = empty
    mappend = union
    mconcat = unions

data IntConvertibleSet k = IntConvertibleSet { proxy :: (Proxy k), toIntSet :: IntSet }



{-# INLINE joinWith #-}
joinWith :: (IntSet -> IntSet -> IntSet) -> IntConvertibleSet k -> IntConvertibleSet k -> IntConvertibleSet k
joinWith f a b = IntConvertibleSet Proxy $ (f `on` toIntSet) a b


toList :: (Pack Int k) => IntConvertibleSet k -> [k]
toList = fmap pack . IntSet.toList . toIntSet

toAscList :: (Pack Int k) => IntConvertibleSet k -> [k]
toAscList = fmap pack . IntSet.toAscList . toIntSet

toDescList :: (Pack Int k) => IntConvertibleSet k -> [k]
toDescList = fmap pack . IntSet.toDescList . toIntSet

fromList :: (Unpack k Int) => [k] -> IntConvertibleSet k
fromList = IntConvertibleSet Proxy . IntSet.fromList . (fmap unpack)

fromAscList :: (Unpack k Int) => [k] -> IntConvertibleSet k
fromAscList = IntConvertibleSet Proxy . IntSet.fromAscList . (fmap unpack)

fromDistinctAscList :: (Unpack k Int) => [k] -> IntConvertibleSet k
fromDistinctAscList = IntConvertibleSet Proxy . IntSet.fromDistinctAscList . (fmap unpack)

singleton :: (Unpack k Int) => k -> IntConvertibleSet k
singleton = IntConvertibleSet Proxy . IntSet.singleton . unpack

size :: IntConvertibleSet k -> Int
size = IntSet.size . toIntSet

null :: IntConvertibleSet k -> Bool
null = IntSet.null . toIntSet

member :: Int -> IntConvertibleSet k -> Bool
member k = IntSet.member k . toIntSet

elems :: (Pack Int k) => IntConvertibleSet k -> [k]
elems = fmap pack . IntSet.elems . toIntSet

notMember :: Int -> IntConvertibleSet k -> Bool
notMember k = IntSet.notMember k . toIntSet

insert :: (Unpack k Int) => k -> IntConvertibleSet k -> IntConvertibleSet k
insert k m = IntConvertibleSet Proxy $ IntSet.insert (unpack k) (toIntSet m)

delete :: (Unpack k Int) => k -> IntConvertibleSet k -> IntConvertibleSet k
delete k m = IntConvertibleSet Proxy $ IntSet.delete (unpack k) (toIntSet m)

empty :: (Unpack k Int) => IntConvertibleSet k
empty = IntConvertibleSet Proxy $ IntSet.empty

union :: IntConvertibleSet k -> IntConvertibleSet k -> IntConvertibleSet k
union = joinWith IntSet.union

filter :: (Pack Int k) => (k -> Bool) -> IntConvertibleSet k -> IntConvertibleSet k
filter f m = IntConvertibleSet Proxy $ IntSet.filter (f.pack) (toIntSet m)

partition :: (Pack Int k) => (k -> Bool) -> IntConvertibleSet k -> (IntConvertibleSet k, IntConvertibleSet k)
partition f m = uncurry ((,) `on` IntConvertibleSet Proxy) $ IntSet.partition (f.pack) (toIntSet m)

split :: (Unpack k Int) => k -> IntConvertibleSet k -> (IntConvertibleSet k, IntConvertibleSet k)
split s m = uncurry ((,) `on` IntConvertibleSet Proxy) $ IntSet.split (unpack s) (toIntSet m)

unions :: [IntConvertibleSet k] -> IntConvertibleSet k
unions = IntConvertibleSet Proxy . IntSet.unions . (fmap toIntSet)

intersection :: IntConvertibleSet k -> IntConvertibleSet k -> IntConvertibleSet k
intersection = joinWith IntSet.intersection

isSubsetOf :: IntConvertibleSet k -> IntConvertibleSet k -> Bool
isSubsetOf = IntSet.isSubsetOf `on` toIntSet

isProperSubsetOf :: IntConvertibleSet k -> IntConvertibleSet k -> Bool
isProperSubsetOf = IntSet.isProperSubsetOf `on` toIntSet

map :: (Packable k Int) => (k -> k) -> IntConvertibleSet k -> IntConvertibleSet k
map f m = IntConvertibleSet Proxy $ IntSet.map (unpack.f.pack) (toIntSet m)

foldr :: (Pack Int k) => (k -> b -> b) -> b -> IntConvertibleSet k -> b
foldr agg v m = IntSet.foldr (agg.pack) v (toIntSet m)

foldl :: (Pack Int k) => (b -> k -> b) -> b -> IntConvertibleSet k -> b
foldl agg v m = IntSet.foldl agg' v (toIntSet m)
  where agg' b k = agg b (pack k)

foldr' :: (Pack Int k) => (k -> b -> b) -> b -> IntConvertibleSet k -> b
foldr' agg v m = IntSet.foldr' (agg.pack) v (toIntSet m)

foldl' :: (Pack Int k) => (b -> k -> b) -> b -> IntConvertibleSet k -> b
foldl' agg v m = IntSet.foldl' agg' v (toIntSet m)
  where agg' b k = agg b (pack k)

infixl 9 \\
(\\), difference :: IntConvertibleSet k -> IntConvertibleSet k -> IntConvertibleSet k
difference = joinWith IntSet.difference
(\\) = difference
