{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

module Data.Containers.Instances where

import           Flowbox.Prelude hiding (Indexable, index)
import           Data.Containers.Class

import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.Vector (Vector)
import qualified Data.Vector as Vector



-- TF instances
type instance ElementOf (IntMap a) = a
type instance ElementOf (Map  k a) = a

type instance IndexOf el (IntMap a) = Int
type instance IndexOf el (Map  k a) = k

type instance ElementByIdx idx (IntMap a) = a
type instance ElementByIdx idx (Map  k a) = a


-- vector instances

type instance ElementOf        (Vector a) = a
type instance ElementByIdx idx (Vector a) = a
type instance IndexOf      el  (Vector a) = Int

instance            Measurable          (Vector a)       where size                    = fromIntegral . Vector.length
instance            Container           (Vector a) Int a where elems                   = Vector.toList
                                                               indexes               v = [0 .. size v - 1]
instance (a ~ b) => Appendable          (Vector b) Int a where append              a v = (v', Vector.length v' - 1) where v' = Vector.snoc v a
instance (a ~ b) => Prependable         (Vector b) Int a where prepend             a v = (v', 0)             where v' = Vector.cons a v
instance (a ~ b) => Indexable           (Vector b) Int a where index           idx   v = (Vector.!?)          v idx
instance (a ~ b) => UnsafeIndexable     (Vector b) Int a where unsafeIndex     idx   v = (Vector.!)           v idx
instance (a ~ b) => UncheckedIndexable  (Vector b) Int a where uncheckedIndex  idx   v = (Vector.unsafeIndex) v idx
instance (a ~ b) => UnsafeInsertable    (Vector b) Int a where unsafeInsert    idx a v = (Vector.//)        v [(idx,a)]
instance (a ~ b) => UncheckedInsertable (Vector b) Int a where uncheckedInsert idx a v = (Vector.unsafeUpd) v [(idx,a)]
instance (a ~ b) => Updatable           (Vector b) Int a where update          idx a v = if idx >= 0 && idx < Vector.length v
                                                                                             then Just $ uncheckedInsert idx a v
                                                                                             else Nothing


-- list instances

type instance ElementOf        [a] = a
type instance ElementByIdx idx [a] = a
type instance IndexOf       el [a] = Int


instance            Measurable   [b]       where size             = fromIntegral . length
instance            Container    [b] Int b where elems            = id
                                                 indexes        l = [0 .. size l -1]
instance (a ~ b) => Appendable   [b] Int a where append       a l = case l of []     -> ([a], 0)
                                                                              (x:xs) -> (x:l', idx' + 1) where
                                                                                  (l', idx') = append  a xs
instance (a ~ b) => Prependable  [b] Int a where prepend      a l = (a:l, 0)
instance (a ~ b) => Indexable    [b] Int a where index    idx   l = case (idx, l) of
                                                                        (0, (x:_))  -> Just x
                                                                        (_, (_:xs)) -> index (idx - 1) xs
                                                                        _           -> Nothing
instance (a ~ b) => Updatable    [b] Int a where update   idx a l = case (idx, l) of
                                                                        (0, (x:xs)) -> Just $ a : xs
                                                                        (_, (x:xs)) -> (x:) <$> update (idx - 1) a xs
                                                                        _           -> Nothing


-- missing instances

instance Default (Vector a) where def = mempty
