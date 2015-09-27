{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Container (
    module Data.Container,
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
--import           Data.Container.Class as X -- hiding ( size, elems, indexes, append', append, prepend', prepend
----                                                   , update, insert, unsafeInsert, index
----                                                   , unsafeIndex
----                                                   )
--import qualified Data.Container.Class as Class
import           Data.Container.Instances ()
import           Data.Container.Interface as X


--size :: (HasContainer a cont, Measurable cont, Integral i) => a -> i
--size = Class.size . view container

--elems :: (HasContainer a cont, Container cont idx el) => a -> [el]
--elems = Class.elems . view container

--indexes :: (HasContainer a cont, Container cont idx el) => a -> [idx]
--indexes = Class.indexes . view container

--append' :: (Appendable' cont idx el, HasContainer a cont) => el -> a -> (a, idx)
--append' el = mapOver container $ Class.append' el

--prepend' :: (Prependable cont idx el, HasContainer a cont) => el -> a -> (a, idx)
--prepend' el = mapOver container $ Class.prepend' el

--append :: (Appendable cont el, HasContainer a cont) => el -> a -> a
--append el = container %~ Class.append el

--prepend :: (Prependable cont idx el, HasContainer a cont) => el -> a -> a
--prepend el = container %~ Class.prepend el

--update :: (Updatable cont idx el, HasContainer a cont) => idx -> el -> a -> a
--update idx el = container %~ Class.update idx el

--insert :: (Insertable cont idx el, HasContainer a cont) => idx -> el -> a -> a
--insert idx el = container %~ Class.insert idx el

--unsafeInsert :: (Insertable cont idx el, HasContainer a cont) => idx -> el -> a -> a
--unsafeInsert idx el = container %~ Class.unsafeInsert idx el

--index :: (Indexable cont idx el, HasContainer a cont) => idx -> a -> el
--index idx cont = Class.index idx $ cont ^. container

