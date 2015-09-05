{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Containers.Interface ( module Data.Containers.Interface
                                 , module X
                                 ) where

import Prologue hiding (Indexable, Ixed)

import qualified Data.Containers.Class as Class
import           Data.Containers.Class as X     hiding (appendX, minIndex, maxIndex, size, freeIxes, usedIxes, indexes,alloc, append, prepend, grow, index, add, remove, reserve, release, expand, insert, singleton, Measurable, MinIndexed, MaxIndexed, Singleton, Allocable, Growable, Expandable, Appendable, Prependable, Addable, Removable, Indexable, Insertable, Reservable, Releasable, TracksElems, TracksIxes, TracksFreeIxes, TracksUsedIxes)
import           Data.Containers.Poly {- x -}
import           Data.Typeable

import           Data.TypeLevel.List (In)



type Measurable     cont = MeasurableT     EmptyStarLst cont
type MinIndexed     cont = MinIndexedT     EmptyStarLst cont
type MaxIndexed     cont = MaxIndexedT     EmptyStarLst cont
type Singleton      cont = SingletonT      EmptyStarLst cont
type Allocable      cont = AllocableT      EmptyStarLst cont
type Growable       cont = GrowableT       EmptyStarLst cont
type Expandable     cont = ExpandableT     EmptyStarLst cont
type Appendable     cont = AppendableT     EmptyStarLst cont
type Prependable    cont = PrependableT    EmptyStarLst cont
type Addable        cont = AddableT        EmptyStarLst cont
type Removable      cont = RemovableT      EmptyStarLst cont
type Indexable      cont = IndexableT      EmptyStarLst cont
type Insertable     cont = InsertableT     EmptyStarLst cont
type Reservable     cont = ReservableT     EmptyStarLst cont
type Releasable     cont = ReleasableT     EmptyStarLst cont
type TracksElems    cont = TracksElemsT    EmptyStarLst cont
type TracksIxes     cont = TracksIxesT     EmptyStarLst cont
type TracksFreeIxes cont = TracksFreeIxesT EmptyStarLst cont
type TracksUsedIxes cont = TracksUsedIxesT EmptyStarLst cont


type MeasurableT     q cont = InstX Class.Measurable     q cont
type MinIndexedT     q cont = InstX Class.MinIndexed     q cont
type MaxIndexedT     q cont = InstX Class.MaxIndexed     q cont
type SingletonT      q cont = InstX Class.Singleton      q cont
type AllocableT      q cont = InstX Class.Allocable      q cont
type GrowableT       q cont = InstX Class.Growable       q cont
type ExpandableT     q cont = InstX Class.Expandable     q cont
type AppendableT     q cont = InstX Class.Appendable     q cont
type PrependableT    q cont = InstX Class.Prependable    q cont
type AddableT        q cont = InstX Class.Addable        q cont
type RemovableT      q cont = InstX Class.Removable      q cont
type IndexableT      q cont = InstX Class.Indexable      q cont
type InsertableT     q cont = InstX Class.Insertable     q cont
type ReservableT     q cont = InstX Class.Reservable     q cont
type ReleasableT     q cont = InstX Class.Releasable     q cont
type TracksElemsT    q cont = InstX Class.TracksElems    q cont
type TracksIxesT     q cont = InstX Class.TracksIxes     q cont
type TracksFreeIxesT q cont = InstX Class.TracksFreeIxes q cont
type TracksUsedIxesT q cont = InstX Class.TracksUsedIxes q cont


type AppendableXT     q cont = InstX Class.AppendableX     q cont


class ResultApp (b :: Bool) cont k t g | b k -> cont, b cont g -> k, b g -> t, b k t -> g where
    resultApp :: Proxy b -> Lens' t cont -> (cont -> k) -> (t -> g)
                          --    b     cont k        t    g
instance              ResultApp False cont cont     t    t  where resultApp _ = (%~)
instance Functor m => ResultApp True  cont (m cont) t (m t) where resultApp _ = ($)


-- === Finite ===

--class Measurable     q m cont        size  | q m cont        -> size  where size      :: InstModsX Measurable      q m cont ->              cont -> size

ssize :: (HasContainer t cont, Class.Measurable q m cont size) => InstModsX Class.Measurable q m cont -> t -> size
ssize q c = Class.size q $ view container c

--size2     :: (HasContainer t cont, MeasurableT     mods cont        size)  => Func mods (              t -> size  ) {- where -}; size2     = modFuncX (\q -> Class.size q . view container)
size2     :: (HasContainer t cont, MeasurableT     mods cont        size)  => Func mods (              t -> size  ) {- where -}; size2     = modFuncX $ fmap (. view container) Class.size

size      :: (HasContainer t cont, MeasurableT     mods cont        size ) => Func mods (              t -> size  ) {- where -}; size      = modFuncX $ (. view container) <$> Class.size
minIndex  :: (HasContainer t cont, MinIndexedT     mods cont        bound) => Func mods (              t -> bound ) {- where -}; minIndex  = modFuncX $ (. view container) <$> Class.minIndex
maxIndex  :: (HasContainer t cont, MaxIndexedT     mods cont        bound) => Func mods (              t -> bound ) {- where -}; maxIndex  = modFuncX $ (. view container) <$> Class.maxIndex

-- === Construction ===
singleton ::                       SingletonT      mods cont     el        => Func mods (        el -> cont       ) {- where -}; singleton = modFuncX Class.singleton
alloc     ::                       AllocableT      mods cont               => Func mods ( Int       -> cont       ) {- where -}; alloc     = modFuncX Class.alloc
grow      :: (HasContainer t cont, GrowableT       mods cont        cont') => Func mods ( Int       -> t -> cont' ) {- where -}; grow      = modFuncX $ (fmap . fmap) (. view container) Class.grow
expand    :: (HasContainer t cont, ExpandableT     mods cont        cont') => Func mods (              t -> cont' ) {- where -}; expand    = modFuncX $ fmap (. view container) Class.expand

expand2   :: (HasContainer t cont, ExpandableT     q cont           cont)             => Func q (              t ->   t ) {- where -}; expand2    = modFuncX $ \q t -> t & container %~ Class.expand q
expand3   :: (HasContainer t cont, ExpandableT     q cont        (f cont), Functor f) => Func q (              t -> f t ) {- where -}; expand3    = modFuncX $ \q t -> t & container (Class.expand q)

expand4   :: (HasContainer t cont, ExpandableT     q cont        k, ResultApp (In Ixed q) cont k t t', t' ~ MonoResult q t) => Func q (              t -> t' ) {- where -}; expand4    = modFuncX $ \q -> resultApp (checkQuery $ query q) container (Class.expand q)


--foo :: _ => _
--foo v = ixed expand4 v

-- === Concatenation ===
appendX   :: AppendableXT    mods cont     el       => Func mods (        el -> cont -> MonoResultEl mods cont el ) {- where -}; appendX    = modFuncX Class.appendx

append    :: (HasContainer t cont, AppendableT     mods cont     el cont') => Func mods (        el -> t -> cont' ) {- where -}; append    = modFuncX $ (fmap . fmap) (. view container) Class.append
prepend   :: (HasContainer t cont, PrependableT    mods cont     el cont') => Func mods (        el -> t -> cont' ) {- where -}; prepend   = modFuncX $ (fmap . fmap) (. view container) Class.prepend
add       :: (HasContainer t cont, AddableT        mods cont     el cont') => Func mods (        el -> t -> cont' ) {- where -}; add       = modFuncX $ (fmap . fmap) (. view container) Class.add
remove    :: (HasContainer t cont, RemovableT      mods cont     el cont') => Func mods (        el -> t -> cont' ) {- where -}; remove    = modFuncX $ (fmap . fmap) (. view container) Class.remove

-- === Modification ===
index     :: IndexableT      mods cont idx    el    => Func mods ( idx       -> cont -> el    ) {- where -}; index     = modFuncX Class.index
insert    :: InsertableT     mods cont idx el cont' => Func mods ( idx -> el -> cont -> cont' ) {- where -}; insert    = modFuncX Class.insert
reserve   :: ReservableT     mods cont        cont' => Func mods (              cont -> cont' ) {- where -}; reserve   = modFuncX Class.reserve
release   :: ReleasableT     mods cont idx    cont' => Func mods ( idx       -> cont -> cont' ) {- where -}; release   = modFuncX Class.release

-- === Indexing ===
indexes   :: TracksIxesT     mods cont        ixes  => Func mods (             cont -> ixes   ) {- where -}; indexes   = modFuncX Class.indexes
freeIxes  :: TracksFreeIxesT mods cont        ixes  => Func mods (             cont -> ixes   ) {- where -}; freeIxes  = modFuncX Class.freeIxes
usedIxes  :: TracksUsedIxesT mods cont        ixes  => Func mods (             cont -> ixes   ) {- where -}; usedIxes  = modFuncX Class.usedIxes




--foox :: _ => _
--foox = ixed appendX 'a'