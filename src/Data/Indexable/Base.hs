{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

module Data.Indexable.Base where

import Prelude

import           Data.Vector         hiding ((++), splitAt, length)
import qualified Data.Vector         as Vector
import           Data.Vector.Mutable hiding (splitAt, length)
import           Data.Maybe          (fromJust)
import qualified Data.Map            as Map
import           Data.Map            (Map)
import qualified Data.IntMap         as IntMap
import           Data.IntMap         (IntMap)
import           Control.Lens        (Index)
import           Data.Containers     (ElementOf)



-- Unsafe without boundary checking
class UncheckedGetIdx a where
    uncheckedGetIdx :: Index a -> a -> ElementOf a

class UncheckedSetIdx a where
    uncheckedSetIdx :: (Index a) -> (ElementOf a) -> a -> a


-- Unsafe with boundary checking
class CheckedGetIdx a where
    checkedGetIdx :: Index a -> a -> ElementOf a

class CheckedSetIdx a where
    checkedSetIdx :: Index a -> ElementOf a -> a -> a


-- Safe without boundary checking

class GetIdx a where
    getIdx :: Index a -> a -> Maybe (ElementOf a)

class SetIdx a where
    setIdx :: Index a -> ElementOf a -> a -> Maybe a

-- Index checking

class LastIdx a where
    lastIdx :: a -> Index a

-- General indexable class

type IndexableCtx a = ( UncheckedGetIdx a, UncheckedSetIdx a, CheckedGetIdx a, CheckedSetIdx a
                      , GetIdx a, SetIdx a, LastIdx a)
class IndexableCtx a => Indexable a

instance IndexableCtx a => Indexable a

-- Default instances

instance {-# OVERLAPPABLE #-} CheckedGetIdx a => UncheckedGetIdx a where
    uncheckedGetIdx = checkedGetIdx

instance {-# OVERLAPPABLE #-} GetIdx a => CheckedGetIdx a where
    checkedGetIdx i a = fromJust $ getIdx i a

instance {-# OVERLAPPABLE #-} CheckedSetIdx a => UncheckedSetIdx a where
    uncheckedSetIdx = checkedSetIdx

instance {-# OVERLAPPABLE #-} SetIdx a => CheckedSetIdx a where
    checkedSetIdx i v a = fromJust $ setIdx i v a

-- === Instances ===

-- Lists

instance GetIdx [a] where
    getIdx 0 [a]    = Just a
    getIdx a []     = Nothing
    getIdx i (x:xs) = getIdx (i-1) xs

instance SetIdx [a] where
    setIdx 0 a b      = Just (a:b)
    setIdx i a []     = Nothing
    setIdx i a (x:xs) = fmap (x :) $ setIdx (i-1) a xs


-- Vectors

instance UncheckedGetIdx (Vector a) where uncheckedGetIdx i v   = Vector.unsafeIndex v i
instance UncheckedSetIdx (Vector a) where uncheckedSetIdx i a v = Vector.unsafeUpd v [(i,a)]
instance CheckedGetIdx   (Vector a) where checkedGetIdx   i v   = (Vector.!) v i
instance CheckedSetIdx   (Vector a) where checkedSetIdx   i a v = (Vector.//) v [(i,a)]
instance GetIdx          (Vector a) where getIdx          i v   = (Vector.!?) v i
instance SetIdx          (Vector a) where setIdx          i a v = if i < Vector.length v then Just $ uncheckedSetIdx i a v
                                                                                         else Nothing
instance LastIdx (Vector a) where lastIdx a = Vector.length a - 1

instance LastIdx [a] where lastIdx a = length a - 1

-- Maps

instance Ord k => GetIdx (Map k v) where getIdx       = Map.lookup
instance Ord k => SetIdx (Map k v) where setIdx k v a = Just $ Map.insert k v a


-- IntMaps

instance GetIdx (IntMap a) where getIdx       = IntMap.lookup
instance SetIdx (IntMap a) where setIdx k v a = Just $ IntMap.insert k v a
