{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.Containers.Class where

import           Flowbox.Prelude hiding (Indexable, index)
import           Data.Maybe             (fromJust)

import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.Vector (Vector)
import qualified Data.Vector as Vector


--- === HasContainer ===

class HasContainer a cont | a -> cont where
    container :: Lens' a cont

instance HasContainer [a]           [a]           where container = id
instance HasContainer (Vector a)    (Vector a)    where container = id


--- === Containers ===

type family ElementOf        cont
type family IndexOf      el  cont
type family ElementByIdx idx cont
type family IdxType      idx


class Measurable a where size :: Integral i => a -> i

class (IndexOf el cont ~ idx, ElementByIdx idx cont ~ el, Measurable cont) => Container cont idx el where
    elems   :: cont -> [el]
    indexes :: cont -> [idx]

class Container cont idx el => Appendable          cont idx el where append          :: el -> cont -> (cont, idx)
class Container cont idx el => Prependable         cont idx el where prepend         :: el -> cont -> (cont, idx)
class Container cont idx el => Updatable           cont idx el where update          :: idx -> el -> cont -> Maybe cont
class Container cont idx el => Insertable          cont idx el where insert          :: idx -> el -> cont -> cont
class Container cont idx el => UnsafeInsertable    cont idx el where unsafeInsert    :: idx -> el -> cont -> cont
class Container cont idx el => UncheckedInsertable cont idx el where uncheckedInsert :: idx -> el -> cont -> cont
class Container cont idx el => Indexable           cont idx el where index           :: idx -> cont -> Maybe el
class Container cont idx el => UnsafeIndexable     cont idx el where unsafeIndex     :: idx -> cont -> el
class Container cont idx el => UncheckedIndexable  cont idx el where uncheckedIndex  :: idx -> cont -> el

class Requestable p where request :: p -> (ElementOf p, p)
class Releasable  p where release :: ElementOf p -> p -> p

instance {-# OVERLAPPABLE #-} Indexable        cont idx el => UnsafeIndexable     cont idx el where unsafeIndex     = fromJust .: index
instance {-# OVERLAPPABLE #-} UnsafeIndexable  cont idx el => UncheckedIndexable  cont idx el where uncheckedIndex  = unsafeIndex
instance {-# OVERLAPPABLE #-} Updatable        cont idx el => UnsafeInsertable    cont idx el where unsafeInsert    = fromJust .:. update
instance {-# OVERLAPPABLE #-} UnsafeInsertable cont idx el => UncheckedInsertable cont idx el where uncheckedInsert = unsafeInsert

-- utils

append_ :: Appendable cont idx el => el -> cont -> cont
append_ = fst .: append

prepend_ :: Prependable cont idx el => el -> cont -> cont
prepend_ = fst .: prepend

