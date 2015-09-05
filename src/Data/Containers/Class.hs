{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE RankNTypes      #-}

{-# LANGUAGE PolyKinds      #-}


module Data.Containers.Class where

import           Prologue        hiding (Indexable, index, Bounded, Ixed)
import           Data.Maybe             (fromJust)
import           Data.Typeable

import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Data.TypeLevel.List (In)

import Data.TypeLevel.Bool

import Data.Containers.Poly

--- === Bounded ===


--class (Ord idx, Enum idx) => Bounded cont idx where
--    minIndexIdx :: cont -> idx
--    maxBoundIdx :: cont -> idx

--minIdx :: Bounded cont (IndexOf' cont) => cont -> IndexOf' cont
--minIdx = minIndexIdx

--maxIdx :: Bounded cont (IndexOf' cont) => cont -> IndexOf' cont
--maxIdx = maxBoundIdx

--- === HasContainer ===

class HasContainer a cont | a -> cont where
    container :: Lens' a cont

instance HasContainer [a]           [a]           where container = id

type IsContainer a = HasContainer a a


--- === Containers ===

type family ElementOf        cont
type family IndexOf      el  cont
type family ElementByIx idx cont
type family IxType      idx

type IndexOf' cont = IndexOf (ElementOf cont) cont

--class Measurable a where size :: Integral i => a -> i

-- GHC BUG [WD]: we can remove fundeps here when it will be fixed: https://ghc.haskell.org/trac/ghc/ticket/10778
class (IndexOf el cont ~ idx, ElementByIx idx cont ~ el) => Container cont idx el | el cont -> idx, idx cont -> el where
    --elems   :: cont -> [el]
    --indexes :: cont -> [idx]


-- === Finite ===
class Measurable     q m cont        size  | q m cont        -> size  where size      :: InstModsX Measurable      q m cont ->              cont -> size
class MinIndexed     q m cont        idx   | q m cont        -> idx   where minIndex  :: InstModsX MinIndexed      q m cont ->              cont -> idx
class MaxIndexed     q m cont        idx   | q m cont        -> idx   where maxIndex  :: InstModsX MaxIndexed      q m cont ->              cont -> idx

-- === Construction ===
class Singleton      q m cont     el                                  where singleton :: InstModsX Singleton       q m cont ->        el -> cont
class Allocable      q m cont                                         where alloc     :: InstModsX Allocable       q m cont -> Int ->       cont
class Growable       q m cont        cont' | q m cont        -> cont' where grow      :: InstModsX Growable        q m cont -> Int ->       cont -> cont'
class Expandable     q m cont        cont' | q m cont        -> cont' where expand    :: InstModsX Expandable      q m cont ->              cont -> cont'

-- === Concatenation ===

type family   MonoResultEl (q :: [*])       cont el
type instance MonoResultEl '[]              cont el = cont
type instance MonoResultEl (Ixed      ': q) cont el = (IndexOf el cont, MonoResultEl q cont el)
type instance MonoResultEl (Unchecked ': q) cont el = MonoResultEl q cont el

type family   MonoResult (q :: [*])       cont
type instance MonoResult '[]              cont = cont
type instance MonoResult (Ixed      ': q) cont = (IndexOf' cont, MonoResult q cont)
type instance MonoResult (Unchecked ': q) cont = MonoResult q cont

type family   PolyResultEl (q :: [*])       cont el
type instance PolyResultEl '[]              cont el = cont
type instance PolyResultEl (Ixed      ': q) cont el = ([IndexOf el cont], PolyResultEl q cont el)
type instance PolyResultEl (Unchecked ': q) cont el = PolyResultEl q cont el


class AppendableX    q m cont     el where appendx  :: InstModsX AppendableX      q m cont ->        el -> cont -> MonoResultEl q cont el
class PrependableX   q m cont     el where prependx :: InstModsX PrependableX     q m cont ->        el -> cont -> MonoResultEl q cont el
class AddableX       q m cont     el where addx     :: InstModsX AddableX         q m cont ->        el -> cont -> MonoResultEl q cont el
class RemovableX     q m cont     el where removex  :: InstModsX RemovableX       q m cont ->        el -> cont -> MonoResultEl q cont el

class Appendable     q m cont     el cont' | q m cont     el -> cont' where append    :: InstModsX Appendable      q m cont ->        el -> cont -> cont'
class Prependable    q m cont     el cont' | q m cont     el -> cont' where prepend   :: InstModsX Prependable     q m cont ->        el -> cont -> cont'
class Addable        q m cont     el cont' | q m cont     el -> cont' where add       :: InstModsX Addable         q m cont ->        el -> cont -> cont'
class Removable      q m cont     el cont' | q m cont     el -> cont' where remove    :: InstModsX Removable       q m cont ->        el -> cont -> cont'

-- === Modification ===
class Indexable      q m cont idx    el    | q m cont idx    -> el    where index     :: InstModsX Indexable       q m cont -> idx ->       cont -> el
class Insertable     q m cont idx el cont' | q m cont idx el -> cont' where insert    :: InstModsX Insertable      q m cont -> idx -> el -> cont -> cont'
class Reservable     q m cont        cont' | q m cont        -> cont' where reserve   :: InstModsX Reservable      q m cont ->              cont -> cont'
class Releasable     q m cont idx    cont' | q m cont idx    -> cont' where release   :: InstModsX Releasable      q m cont -> idx ->       cont -> cont'

-- === Indexing ===
class TracksElems    q m cont        els   | q m cont        -> els   where elems     :: InstModsX TracksElems     q m cont ->              cont -> els
class TracksIxes     q m cont        ixes  | q m cont        -> ixes  where indexes   :: InstModsX TracksIxes      q m cont ->              cont -> ixes
class TracksFreeIxes q m cont        ixes  | q m cont        -> ixes  where freeIxes  :: InstModsX TracksFreeIxes  q m cont ->              cont -> ixes
class TracksUsedIxes q m cont        ixes  | q m cont        -> ixes  where usedIxes  :: InstModsX TracksUsedIxes  q m cont ->              cont -> ixes




--Replicable
--head, init, tail, last
--take, drop
--split
--generate
--force?
--update (like in Vector)
--reverse
--elem
--notElem
--find





runMods :: (LstIn (ModsOf cont inst) mods ~ matches) => Mods mods -> (InstMods inst matches -> cont -> out) -> cont -> out
runMods _ f = f InstMods





--moze przez s z typeclass komunikowac kim one sa?

--class b ~ FFoo a => Foo a b where
--    foo :: a -> b

--instance Foo Int String where
--    foo = undefined


----instance Foo t Int where
----    foo = undefined

--type family FFoo a
--type instance FFoo Int = Int
--type instance FFoo String = String

--type instance ModsOf (V.Vector a) Appendable = '[Ixed]

                                                                     --            (cont', tailIdx) = expandIdxed cont

--class (Container cont (IndexOf el cont) el) => Appendable cont     el where append :: el -> cont -> cont
--                                                                            default append :: Appendable' cont (IndexOf el cont) el => el -> cont -> cont
--                                                                            append = fst .: append'

--class Container cont idx el => Prependable         cont idx el where prepend'     :: el -> cont -> (cont, idx)
--class Container cont idx el => Updatable           cont idx el where update       :: idx -> el -> cont -> cont
--class Container cont idx el => Insertable          cont idx el where insert       :: idx -> el -> cont -> cont
--                                                                     unsafeInsert :: idx -> el -> cont -> cont


--class Requestable p where request :: p -> (ElementOf p, p)
--class Releasable  p where release :: ElementOf p -> p -> p

--instance {-# OVERLAPPABLE #-} Indexable cont idx el => Indexable'  cont idx el where unsafeIndex  = index
--instance {-# OVERLAPPABLE #-} Updatable cont idx el => Insertable' cont idx el where unsafeInsert = update

-- utils

--grow :: Growable cont => Int -> cont -> cont
--grow i cont = if i < 0 then error "negative index"
--                       else unsafeGrow i cont

--append :: Appendable' cont idx el => el -> cont -> cont
--append = fst .: append'

--prepend :: Prependable cont idx el => el -> cont -> cont
--prepend = fst .: prepend'

--alloc i = if i < 0 then error $ "negative length " <> show i
--                   else unsafeAlloc i

-- nie powinno byc czegos takiego jak unsafe (metody safe zwracaja maybe) - poniewaz mozna zrobic jedna generalna metode ktora sprawdza czy index jest w zakresach i zwraca maybe (!)


--data Unchecked a = Unchecked a deriving (Show, Functor)

--unsafe       = Unchecked
--unsafely f a = f $ Unchecked a

--instance Wrap    Unchecked where wrap              = Unchecked
--instance Unwrap  Unchecked where unwrap (Unchecked a) = a
--instance Wrapped Unchecked



--instance HasContainer (Unchecked a) (Unchecked a) where container = id


--type instance ElementOf       (Unchecked a) = ElementOf a
--type instance ElementByIx idx (Unchecked a) = ElementByIx idx a
--type instance IndexOf     el  (Unchecked a) = IndexOf el a


--instance Measurable a       => Measurable (Unchecked a) where size = size . unwrap
--instance Container a idx el => Container (Unchecked a) idx el where elems   = elems   . unwrap
--                                                                 indexes = indexes . unwrap

--index2' = index2 emptyOpts


--instance
--instance {-# OVERLAPPABLE #-} (t ~ (f -> g), opts ~ '[]) => FuncBuilder (OptBuilder opts f) (a -> b) where buildFunc = undefined

--instance (t ~ (Mods opts -> a -> b)) => FuncBuilder (OptBuilder opts t) (Mods opts -> a -> b) where buildFunc (OptBuilder f) = f Mods


extendOptBuilder :: Proxy opt -> OptBuilder opts a -> OptBuilder (opt ': opts) a
extendOptBuilder _ (OptBuilder a) = OptBuilder a

type ModConstraint mod = FuncTrans (mod ': opts) f g => OptBuilder opts f -> g


modConstraint :: Proxy mod -> ModConstraint mod
modConstraint = transFunc .: extendOptBuilder

unchecked :: ModConstraint Unchecked
unchecked = modConstraint (Proxy :: Proxy Unchecked)

safe :: ModConstraint Safe
safe = modConstraint (Proxy :: Proxy Safe)

ixed :: ModConstraint Ixed
ixed = modConstraint (Proxy :: Proxy Ixed)





data Safe      = Safe
data Unchecked = Unchecked
data Ixed      = Ixed














class Result2 (i :: Bool) t f g | i t f -> g, i g -> t  where
    result2 :: a ~ ResultElem i f => Proxy i -> Lens (t a') (t a) a' a -> (a' -> f) -> (t a' -> g)

type family ResultElem i a where
    ResultElem True  (f a) = a
    ResultElem False a     = a

instance {-# OVERLAPPABLE #-} Result2 False t a     (t a)     where result2 _ lens f = lens %~ f
instance Functor f         => Result2 True  t (f a) (f (t a)) where result2 _ lens f = lens f

checkQuery :: Proxy q -> Proxy (In Ixed q)
checkQuery _ = Proxy



type Result2' q = Result2 (In Ixed q)

result2' :: (a ~ ResultElem i f, i ~ In Ixed q, Result2 i t f g) => Proxy q -> Lens (t a') (t a) a' a -> (a' -> f) -> (t a' -> g)
result2' = result2 . checkQuery



wrappedResult :: (a ~ ResultElem i f, i ~ In Ixed q, Result2 i t f g, Wrapped t) => Proxy q -> (a' -> f) -> (t a' -> g)
wrappedResult q = result2' q wrapped


runWrapped0 s f = wrappedResult (query s) $   f (polySpecX s)
runWrapped1 s f = wrappedResult (query s) .   f (polySpecX s)
runWrapped2 s f = wrappedResult (query s) .:  f (polySpecX s)
runWrapped3 s f = wrappedResult (query s) .:. f (polySpecX s)
runWrapped4 s f = wrappedResult (query s) .:: f (polySpecX s)


--type InstFunc2 (inst :: [Bool] -> * -> k) mods cont sig = InstMods2 inst (LstIn (ModsOf cont inst) mods) cont -> sig
--type SchemeBuilder2 cont mods sig scheme = FuncBuilder (Mods mods -> sig) scheme => InstFunc2 inst mods cont sig -> scheme



--buildScheme2 :: SchemeBuilder2 inst cont mods out scheme



--modFunc2 = transFunc . buildScheme2




--type InstFunc2 (inst :: [Bool] -> * -> k) mods cont sig = InstMods2 inst (LstIn (ModsOf cont inst) mods) cont -> sig

--type InstFunc2 inst mods cont sig = InstMods2 inst (LstIn (ModsOf cont inst) mods) cont -> sig

--buildScheme2 :: (FuncBuilder (Mods mods -> sig) a) => (InstMods2 inst (LstIn (ModsOf cont inst) mods) cont -> sig) -> a
--buildScheme2 :: SchemeBuilder2 cont mods sig scheme

