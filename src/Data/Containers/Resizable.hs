{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}

module Data.Containers.Resizable where

import Prologue              hiding (Indexable, index, Bounded)
import Data.Containers.Class
import Data.Typeable
import qualified Data.Containers.Interface as I
import           Data.Containers.Poly {- x -}
import           Data.TypeLevel.List (In)


type Result q s = Result2' q (Resizable s)



-- === Resizable wrapper

data Resizable style a = Resizable !style !a deriving (Show)

instance              Unwrap  (Resizable s) where  unwrap (Resizable _ a) = a
instance Default s => Wrap    (Resizable s) where    wrap = Resizable def
instance              Wrapped (Resizable s) where wrapped = lens unwrap $ \(Resizable s _) a -> Resizable s a


instance (Default s, Monoid a) => Monoid (Resizable s a) where
    mempty                                   = Resizable def mempty
    mappend (Resizable s a) (Resizable _ a') = Resizable s (a <> a')

instance (Default s, Monoid a) => Default (Resizable s a) where
    def = Resizable def mempty

instance (IsList a, Default s) => IsList (Resizable s a) where
    type Item (Resizable s a) = Item a
    fromList = Resizable def . fromList



-- === TF Instances ===

instance HasContainer (Resizable s a) (Resizable s a) where container = id

type instance ElementOf        (Resizable s a) = ElementOf       a
type instance ElementByIx  idx (Resizable s a) = ElementByIx idx a
type instance IndexOf      el  (Resizable s a) = IndexOf     el  a


type instance ModsOf (Resizable s a) inst = '[]

-- === Finite ===

-- [+] Measurable
-- [+] MinIndexed
-- [+] MaxIndexed

instance I.MeasurableT q a size => Measurable q mods (Resizable s a) size where size     spec = size     (polySpecX spec) . unwrap
instance I.MinIndexedT q a idx  => MinIndexed q mods (Resizable s a) idx  where minIndex spec = minIndex (polySpecX spec) . unwrap
instance I.MaxIndexedT q a idx  => MaxIndexed q mods (Resizable s a) idx  where maxIndex spec = maxIndex (polySpecX spec) . unwrap

---- === Construction ===

---- [+] Singleton
---- [+] Allocable
---- [+] Growable
---- [+] Expandable

instance (Default s, I.SingletonT  q a el)                       => Singleton  q mods (Resizable s a) el  where singleton spec   = wrap . singleton (polySpecX spec)
instance (Default s, I.AllocableT  q a   )                       => Allocable  q mods (Resizable s a)     where alloc     spec   = wrap . alloc     (polySpecX spec)
instance (I.GrowableT q a a', Result q s a' out)                 => Growable   q mods (Resizable s a) out where grow      spec   = runWrapped1 spec grow
instance (I.GrowableT q a a', Result q s a' out, ResizeStep s a) => Expandable q mods (Resizable s a) out where expand    spec c = grow (rebaseSpecX spec) (resizeStep c) c



---- === Concatenation ===
---- [+] Appendable
---- [+] Prependable
---- [+] Addable
---- [+] Removable

--instance (AppendableX q (LstIn (ModsOf a AppendableX) q) a el, Result2 (In I.Ixed q) (Resizable s) (MonoResultEl q a el) (MonoResultEl q (Resizable s a) el) ) => AppendableX  q mods (Resizable s a) el where appendx spec = runWrapped1 spec appendx


instance (I.AppendableT  q a el a', Result q s a' out) => Appendable  q mods (Resizable s a) el out where append  spec = runWrapped1 spec append
instance (I.PrependableT q a el a', Result q s a' out) => Prependable q mods (Resizable s a) el out where prepend spec = runWrapped1 spec prepend
instance (I.AddableT     q a el a', Result q s a' out) => Addable     q mods (Resizable s a) el out where add     spec = runWrapped1 spec add
instance (I.RemovableT   q a el a', Result q s a' out) => Removable   q mods (Resizable s a) el out where remove  spec = runWrapped1 spec remove


-- === Modification ===

-- [+] Indexable
-- [+] Insertable
-- [ ] Reservable
-- [ ] Releasable

instance  I.IndexableT  q a idx el                                                      => Indexable  q mods (Resizable s a) idx el     where index  spec idx   = index (polySpecX spec) idx . unwrap
instance (I.InsertableT q a idx el a', Resize s a idx, Result2' q (Resizable s) a' out) => Insertable q mods (Resizable s a) idx el out where insert spec idx a = runWrapped2 spec insert idx a . resize idx


-- === Indexing ===

-- [+] TracksElems
-- [+] TracksIxes
-- [-] TracksFreeIxes
-- [-] TracksUsedIxes

instance I.TracksElemsT q a elems => TracksElems q mods (Resizable s a) elems where elems   spec = elems   (polySpecX spec) . unwrap
instance I.TracksIxesT  q a ixes  => TracksIxes  q mods (Resizable s a) ixes  where indexes spec = indexes (polySpecX spec) . unwrap


----------------------------------


data Minimal     = Minimal   deriving (Show)
data Exponential = Exponential deriving (Show)

instance Default Minimal     where def = Minimal
instance Default Exponential where def = Exponential

--class Resize2 style cont where
--    resizeAmount :: Proxy style -> cont -> Int

class                                                                                               Resize style       cont idx where resize     :: idx -> Resizable style cont -> Resizable style cont
instance (                       I.MaxIndexed cont idx, I.Growable cont cont, Enum idx, Ord idx) => Resize Minimal     cont idx where resize idx c = if isOverBounds idx c then flip I.grow c $ ((-) `on` fromEnum) idx $ I.maxIndex c else c
instance (I.Measurable cont Int, I.MaxIndexed cont idx, I.Growable cont cont, Enum idx, Ord idx) => Resize Exponential cont idx where resize idx c = if isOverBounds idx c then flip I.grow c $ dupCheckSize (fromEnum idx) (I.size c) - I.size c else c

class                             ResizeStep style       cont where resizeStep :: Resizable style cont -> Int
instance                          ResizeStep Minimal     cont where resizeStep c = 1
instance I.Measurable cont Int => ResizeStep Exponential cont where resizeStep c = checkZeroSize $ I.size c


checkZeroSize s = if s == 0 then 1 else s

dupCheckSize i = dupSize i . checkZeroSize

dupSize i size = if i >= size then dupSize i (2 * size)
                              else size


isOverBounds :: (Ord idx, I.MaxIndexed cont idx, HasContainer t cont) => idx -> t -> Bool
isOverBounds idx cont = idx > I.maxIndex cont
