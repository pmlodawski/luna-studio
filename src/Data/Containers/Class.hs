{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module Data.Containers.Class where

import           Prologue        hiding (Indexable, index, Bounded)
import           Data.Maybe             (fromJust)
import           Data.Typeable

import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.Vector (Vector)
import qualified Data.Vector as Vector


--- === Bounded ===


class (Ord idx, Enum idx) => Bounded cont idx where
    minBoundIdx :: cont -> idx
    maxBoundIdx :: cont -> idx

minIdx :: Bounded cont (IndexOf' cont) => cont -> IndexOf' cont
minIdx = minBoundIdx

maxIdx :: Bounded cont (IndexOf' cont) => cont -> IndexOf' cont
maxIdx = maxBoundIdx

--- === HasContainer ===

class HasContainer a cont | a -> cont where
    container :: Lens' a cont

instance HasContainer [a]           [a]           where container = id
instance HasContainer (Vector a)    (Vector a)    where container = id


--- === Containers ===

type family ElementOf        cont
type family IndexOf      el  cont
type family ElementByIx idx cont
type family IxType      idx

type IndexOf' cont = IndexOf (ElementOf cont) cont

class Measurable a where size :: Integral i => a -> i

-- GHC BUG [WD]: we can remove fundeps here when it will be fixed: https://ghc.haskell.org/trac/ghc/ticket/10778
class (IndexOf el cont ~ idx, ElementByIx idx cont ~ el, Measurable cont) => Container cont idx el | el cont -> idx, idx cont -> el where
    elems   :: cont -> [el]
    indexes :: cont -> [idx]

--zmienic remove na remove po indeach w innej typeclassie

class Container cont idx el => SetLike'            cont idx el where add'    :: el -> cont -> (cont, idx)
                                                                     remove' :: el -> cont -> (cont, idx)

class Container cont idx el => Erasable'           cont idx el where unsafeErase' :: idx -> cont -> (cont, el)
class                          Erasable            cont idx    where unsafeErase  :: idx -> cont -> cont
                                                                     default unsafeErase :: Erasable' cont idx el => idx -> cont -> cont
                                                                     unsafeErase = fst .: unsafeErase'


class                          SetLike             cont     el where add    :: el -> cont -> cont
                                                                     remove :: el -> cont -> cont

                                                                     default add :: SetLike' cont idx el => el -> cont -> cont
                                                                     add = fst .: add'

                                                                     default remove :: SetLike' cont idx el => el -> cont -> cont
                                                                     remove = fst .: add'

class                          Sparse              cont idx    where freeIxs :: cont -> [idx]
                                                                     usedIxs :: cont -> [idx]
                                                                     freeIxs _ = []
                                                                     usedIxs _ = []

class                          Reservable          cont idx    where reserveIx  :: cont -> (idx, cont)

class                          Releasable          cont idx    where releaseIx  :: idx  -> cont -> cont

class                          Allocable           cont        where -- replicate       :: Int -> el   -> cont
                                                                     unsafeAlloc  :: Int         -> cont


class                          Singleton           cont     el where singleton :: el -> cont
                                                                     default singleton :: (Monoid cont, Appendable cont el) => el -> cont
                                                                     singleton a = append a mempty


class                          Growable'           cont idx    where unsafeGrow' :: Int -> cont -> (cont, [idx])
                                                                     --default growIdxedUnchecked :: Expandable cont => Int -> cont -> (idx, cont)
                                                                     --growIdxedUnchecked i cont = (unsafeGrow i cont, lastIndex cont)
                                                                     --growIdxedUnchecked i = growIdxedUnchecked (i - 1) . expand

class                          Growable            cont        where unsafeGrow :: Int -> cont -> cont
                                                                     default unsafeGrow :: Expandable cont => Int -> cont -> cont
                                                                     unsafeGrow 0 = id
                                                                     unsafeGrow i = unsafeGrow (i - 1) . expand


class                          Expandable          cont        where expand :: cont -> cont
                                                                     default expand :: Growable cont => cont -> cont
                                                                     expand = grow 1

class                          Expandable'         cont idx    where expand' :: cont -> (cont, [idx])


class Container cont idx el => Appendable'         cont idx el where append' :: el -> cont -> (cont, idx)
                                                                     --default append' :: (IdxExpandable cont idx, Insertable cont idx el) => el -> cont -> (cont, idx)
                                                                     --append' el cont = (insert tailIdx el cont', tailIdx) where
                                                                     --            (cont', tailIdx) = expandIdxed cont

class (Container cont (IndexOf el cont) el) => Appendable cont     el where append :: el -> cont -> cont
                                                                            default append :: Appendable' cont (IndexOf el cont) el => el -> cont -> cont
                                                                            append = fst .: append'

class Container cont idx el => Prependable         cont idx el where prepend'     :: el -> cont -> (cont, idx)
class Container cont idx el => Updatable           cont idx el where update       :: idx -> el -> cont -> cont
class Container cont idx el => Insertable          cont idx el where insert       :: idx -> el -> cont -> cont
                                                                     unsafeInsert :: idx -> el -> cont -> cont
--class Container cont idx el => Indexable           cont idx el where index        :: idx -> cont -> el
--                                                                     --unsafeIndex  :: idx -> cont -> el

class Container cont idx el => Insertable2 opts cont idx el where insert2 :: Opts opts -> idx -> el -> cont -> cont
class Container cont idx el => Indexable2  opts cont idx el where index2  :: Opts opts -> idx -> cont -> el


--class Requestable p where request :: p -> (ElementOf p, p)
--class Releasable  p where release :: ElementOf p -> p -> p

--instance {-# OVERLAPPABLE #-} Indexable cont idx el => Indexable'  cont idx el where unsafeIndex  = index
--instance {-# OVERLAPPABLE #-} Updatable cont idx el => Insertable' cont idx el where unsafeInsert = update

-- utils

grow :: Growable cont => Int -> cont -> cont
grow i cont = if i < 0 then error "negative index"
                       else unsafeGrow i cont

--append :: Appendable' cont idx el => el -> cont -> cont
--append = fst .: append'

prepend :: Prependable cont idx el => el -> cont -> cont
prepend = fst .: prepend'

alloc i = if i < 0 then error $ "negative length " <> show i
                   else unsafeAlloc i

-- nie powinno byc czegos takiego jak unsafe (metody safe zwracaja maybe) - poniewaz mozna zrobic jedna generalna metode ktora sprawdza czy index jest w zakresach i zwraca maybe (!)


data Unsafe a = Unsafe a deriving (Show, Functor)

unsafe       = Unsafe
unsafely f a = f $ Unsafe a

instance Wrap    Unsafe where wrap              = Unsafe
instance Unwrap  Unsafe where unwrap (Unsafe a) = a
instance Wrapped Unsafe



instance HasContainer (Unsafe a) (Unsafe a) where container = id


type instance ElementOf       (Unsafe a) = ElementOf a
type instance ElementByIx idx (Unsafe a) = ElementByIx idx a
type instance IndexOf     el  (Unsafe a) = IndexOf el a


instance Measurable a       => Measurable (Unsafe a) where size = size . unwrap
instance Container a idx el => Container (Unsafe a) idx el where elems   = elems   . unwrap
                                                                 indexes = indexes . unwrap

index2' = index2 emptyOpts

emptyOpts = Opts :: Opts '[]


class MyIndex f a | a -> f where
    myIndex :: f -> a

class MyIndex2 opts f a | a opts -> f where
    myIndex2 :: OptBuilder opts f -> a

instance {-# OVERLAPPABLE #-} (f ~ a, g ~ b)             => MyIndex (f -> g)            (a -> b)            where myIndex = id
instance {-# OVERLAPPABLE #-} (t ~ (f -> g), opts ~ '[]) => MyIndex (f -> g)            (OptBuilder opts t) where myIndex = OptBuilder

instance                                    (opts ~ opts')       => MyIndex2 opts f  (OptBuilder opts' f) where myIndex2 = id
instance                           (f ~ (Opts opts -> a -> b))   => MyIndex2 opts f  (a -> b)            where myIndex2 (OptBuilder f) = f Opts
--instance
--instance {-# OVERLAPPABLE #-} (t ~ (f -> g), opts ~ '[]) => MyIndex (OptBuilder opts f) (a -> b) where myIndex = undefined

--instance (t ~ (Opts opts -> a -> b)) => MyIndex (OptBuilder opts t) (Opts opts -> a -> b) where myIndex (OptBuilder f) = f Opts

newtype OptBuilder (opts :: [*]) a = OptBuilder a

unsafe2 :: OptBuilder opts a -> OptBuilder (Unsafe2 ': opts) a
unsafe2 (OptBuilder a) = OptBuilder a

unsafe3' :: MyIndex2 (Unsafe2 ': opts) f b => OptBuilder opts f -> b
unsafe3' = myIndex2 . unsafe2

unsafe2' = myIndex2 . unsafe2
--xxx :: _ => _
xindex = myIndex index2

--xxx :: _ => (a -> cont -> el)
--xxx = (unsafe2' . unsafe2') xindex
xxx = unsafe2' xindex


--yyy = unsafe3' unsafe3'

--Trans [Unsafe, Ixed]

data Opts (opts :: [*]) = Opts

data Unsafe2 = Unsafe2
data Ixed    = Ixed