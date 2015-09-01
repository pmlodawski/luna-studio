{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Containers.Resizable where

import Prologue              hiding (Indexable, index, Bounded)
import Data.Containers.Class
import Data.Typeable

data Resizable s a = Resizable { _style :: !s, _cont :: !a} deriving (Show)

makeLenses ''Resizable


instance Unwrap (Resizable s) where unwrap = view cont

instance HasContainer (Resizable s a) (Resizable s a) where container = id


type instance ElementOf       (Resizable s a) = ElementOf a
type instance ElementByIx idx (Resizable s a) = ElementByIx idx a
type instance IndexOf     el  (Resizable s a) = IndexOf el a

instance Bounded a idx => Bounded (Resizable s a) idx where
    minBoundIdx = minBoundIdx . unwrap
    maxBoundIdx = maxBoundIdx . unwrap

instance (Default s, Monoid a) => Monoid (Resizable s a) where
    mempty                                   = Resizable def mempty
    mappend (Resizable s a) (Resizable _ a') = Resizable s (a <> a')

instance Measurable a       =>           Measurable          (Resizable s a)        where size             = size    . unwrap
instance Container a idx el =>           Container           (Resizable s a) idx el where elems            = elems   . unwrap
                                                                                          indexes          = indexes . unwrap

instance Appendable'         a  idx el                      => Appendable'        (Resizable s a)  idx el where append'       = mapOver cont . append'
instance Prependable         a  idx el                      => Prependable        (Resizable s a)  idx el where prepend'      = mapOver cont . prepend'

instance (Indexable2  opts a idx el) => Indexable2  opts (Resizable s a) idx el where index2 opts idx = index2 opts idx . unwrap

instance (Insertable          a  idx el, Resize s         a  idx)                     => Insertable          (Resizable s a)  idx el where insert       idx el = (cont %~ insert idx el) . resize idx
instance (Insertable  (Unsafe a) idx el, Resize s (Unsafe a) idx, Container a idx el) => Insertable  (Unsafe (Resizable s a)) idx el where insert       idx el = (cont $ unsafely (insert idx el)) . unwrap
                                                                                                            --unsafeInsert idx el = cont %~ unsafeInsert idx el
instance (Updatable           a idx el                ) => Updatable           (Resizable s a) idx el where update       idx el = cont %~ update idx el



instance                      Growable' a idx                          => Expandable'        (Resizable Minimal   a) idx where expand'       c    = (c & cont .~ a, ixs) where
                                                                                                                                                    (a, ixs) = unsafeGrow' 1 (c ^. cont)
instance                      (Growable' a idx, Measurable a)          => Expandable'        (Resizable Duplicate a) idx where expand'       c    = (c & cont .~ a, ixs) where
                                                                                                                                                    (a, ixs) = unsafeGrow' (checkBaseSize $ size c) (c ^. cont)
instance {-# OVERLAPPABLE #-} (Growable a              )               => Expandable          (Resizable Minimal   a)     where expand                = grow 1
instance                      (Growable a, Measurable a)               => Expandable          (Resizable Duplicate a)     where expand           cont = grow (size cont) cont
instance                      (Growable   a                          ) => Growable            (Resizable s         a)     where unsafeGrow     i      = cont %~ unsafeGrow i
instance                      (Allocable  a, Default s               ) => Allocable           (Resizable s         a)     where unsafeAlloc           = Resizable def . unsafeAlloc

instance (Erasable' a idx el)                                          => Erasable            (Resizable s a) idx
instance (Erasable' a idx el)                                          => Erasable'           (Resizable s a) idx el      where unsafeErase' idx     c = (c & cont .~ a, el) where
                                                                                                                                                         (a, el) = unsafeErase' idx $ c ^. cont


--class Container cont idx el => Erasable'           cont idx el where                                                              unsafeErase' :: idx -> cont -> (cont, el)


data Minimal   = Minimal   deriving (Show)
data Duplicate = Duplicate deriving (Show)

instance Default Minimal   where def = Minimal
instance Default Duplicate where def = Duplicate

class                                                          Resize style     cont idx where resize :: idx -> Resizable style cont -> Resizable style cont
instance (Growable cont, Bounded cont idx                 ) => Resize Minimal   cont idx where resize idx cont = ifElseId (isOverBounds idx cont) (grow $ ((-) `on` fromEnum) idx $ maxBoundIdx cont) cont
instance (Growable cont, Bounded cont idx, Measurable cont) => Resize Duplicate cont idx where resize idx cont = ifElseId (isOverBounds idx cont) (grow $ dupCheckSize (fromEnum idx) (size cont) - size cont) cont


checkBaseSize size = if size == 0 then 1 else size
dupCheckSize i size = dupSize i $ if size == 0 then 1
                                               else size

dupSize i size = if i >= size then dupSize i (2 * size)
                              else size


isOverBounds :: Bounded cont idx => idx -> cont -> Bool
isOverBounds idx cont = idx > maxBoundIdx cont


dots :: [a -> a] -> (a -> a)
dots (f:[]) = f
dots (f:fs) = f . dots fs