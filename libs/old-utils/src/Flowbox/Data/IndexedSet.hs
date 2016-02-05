---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Data.IndexedSet where

import           Data.List       (sortBy)
import           Data.Ord        (comparing)
import           Data.Set        (Set)
import qualified Data.Set        as Set

import           Flowbox.Prelude hiding (empty, set)



newtype IndexedSet a = IndexedSet (Set (Entry a), Int)
                     deriving (Eq, Show)

instance Default (IndexedSet a) where
    def = empty


newtype Entry a = Entry { unEntry :: (Int, a) }
                deriving (Show)


instance Eq a => Eq (Entry a) where
    (Entry (_, a)) == (Entry (_, b)) = a == b

instance Ord a => Ord (Entry a) where
    compare (Entry (_, a)) (Entry (_, b)) = compare a b


null :: IndexedSet a -> Bool
null (IndexedSet (set, _)) = Set.null set


size :: IndexedSet a -> Int
size (IndexedSet (set, _)) = Set.size set


member :: Ord a => a -> IndexedSet a -> Bool
member a (IndexedSet (set, _)) = Set.member (Entry (0, a)) set


empty :: IndexedSet a
empty = IndexedSet (Set.empty, 0)


insert :: Ord a => a -> IndexedSet a -> IndexedSet a
insert a (IndexedSet (set, i)) = IndexedSet (Set.insert (Entry (i, a)) set, i + 1)


delete :: Ord a => a -> IndexedSet a -> IndexedSet a
delete a (IndexedSet (set, i)) = IndexedSet (Set.delete (Entry (undefined, a)) set, i)


toList :: IndexedSet a -> [a]
toList (IndexedSet (set, _)) = map snd
                             $ sortBy (comparing fst)
                             $ map unEntry
                             $ Set.toList set


fromList :: Ord a => [a] -> IndexedSet a
fromList list = iset where
    set = Set.fromList $ map Entry $ zip [0..] list
    iset = IndexedSet (set, Set.size set)
