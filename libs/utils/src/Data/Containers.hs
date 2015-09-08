{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Containers (
    module Data.Containers,
    module X
) where

import           Flowbox.Prelude hiding (Indexable, index)

import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.Vector (Vector)
import qualified Data.Vector as Vector

import           Data.Maybe (fromJust)
import           Data.Containers.Class as X hiding ( size, elems, indexes, append, append_, prepend, prepend_
                                                   , update, insert, unsafeInsert, uncheckedInsert, index
                                                   , unsafeIndex, uncheckedIndex
                                                   )
import qualified Data.Containers.Class as Class
import           Data.Containers.Instances ()


size :: (HasContainer a cont, Measurable cont, Integral i) => a -> i
size = Class.size . view container

elems :: (HasContainer a cont, Container cont idx el) => a -> [el]
elems = Class.elems . view container

indexes :: (HasContainer a cont, Container cont idx el) => a -> [idx]
indexes = Class.indexes . view container

append :: (Appendable cont idx el, HasContainer a cont) => el -> a -> (a, idx)
append el = mapOver container $ Class.append el

prepend :: (Prependable cont idx el, HasContainer a cont) => el -> a -> (a, idx)
prepend el = mapOver container $ Class.prepend el

append_ :: (Appendable cont idx el, HasContainer a cont) => el -> a -> a
append_ el = container %~ Class.append_ el

prepend_ :: (Prependable cont idx el, HasContainer a cont) => el -> a -> a
prepend_ el = container %~ Class.prepend_ el

update :: (Updatable cont idx el, HasContainer a cont) => idx -> el -> a -> Maybe a
update idx el = container $ Class.update idx el

insert :: (Insertable cont idx el, HasContainer a cont) => idx -> el -> a -> a
insert idx el = container %~ Class.insert idx el

unsafeInsert :: (UnsafeInsertable cont idx el, HasContainer a cont) => idx -> el -> a -> a
unsafeInsert idx el = container %~ Class.unsafeInsert idx el

uncheckedInsert :: (UncheckedInsertable cont idx el, HasContainer a cont) => idx -> el -> a -> a
uncheckedInsert idx el = container %~ Class.uncheckedInsert idx el

index :: (Indexable cont idx el, HasContainer a cont) => idx -> a -> Maybe el
index idx cont = Class.index idx $ cont ^. container

unsafeIndex :: (UnsafeIndexable cont idx el, HasContainer a cont) => idx -> a -> el
unsafeIndex idx cont = Class.unsafeIndex idx $ cont ^. container

uncheckedIndex :: (UncheckedIndexable cont idx el, HasContainer a cont) => idx -> a -> el
uncheckedIndex idx cont = Class.uncheckedIndex idx $ cont ^. container