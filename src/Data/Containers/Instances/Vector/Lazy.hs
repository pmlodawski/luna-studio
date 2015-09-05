{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

module Data.Containers.Instances.Vector.Lazy where

import           Flowbox.Prelude hiding (Indexable, index, Bounded, Ixed, switch)
import           Data.Containers.Class

import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Vector                 as V
import qualified Data.Vector.Mutable         as MV
import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import           Control.Monad.ST
import           Data.Typeable
import qualified Data.Containers.Interface as I
import           Data.Containers.Poly {- x -}

import GHC.Prim


-- === TF Instances ===

instance HasContainer (V.Vector a) (V.Vector a) where container = id

type instance ElementOf        (V.Vector a) = a
type instance ElementByIx  idx (V.Vector a) = a
type instance IndexOf      el  (V.Vector a) = Int



-- === Finite ===

-- [+] Measurable
-- [+] MinBounded
-- [+] MaxBounded

type instance ModsOf (V.Vector a) Measurable  = '[]
type instance ModsOf (V.Vector a) MinIndexed  = '[]
type instance ModsOf (V.Vector a) MaxIndexed  = '[]

instance      Measurable  q mods (V.Vector a) Int where size _ = V.length
instance      MinIndexed  q mods (V.Vector a) Int where minIndex _ _ = 0
instance      MaxIndexed  q mods (V.Vector a) Int where maxIndex _ c = I.size c - 1



-- === Construction ===

-- [+] Singleton
-- [+] Allocable
-- [+] Growable
-- [+] Expandable

type instance ModsOf (V.Vector a) Singleton   = '[]
instance                a ~ a' => Singleton   q mods         (V.Vector a) a' where singleton _ = V.singleton

-- FIXME[wd]: We should check if the Unchecked flag is on and check for the negative number
type instance ModsOf (V.Vector a) Allocable   = '[Unchecked]
instance                          Allocable   q mods         (V.Vector a)    where alloc _ i = runST $ V.unsafeFreeze =<< MV.unsafeNew i

-- FIXME[wd]: We should check if the Unchecked flag is on and check for the negative number
type instance ModsOf (V.Vector a) Growable = '[Unchecked, Ixed]
instance Growable q '[u, True ] (V.Vector a) ([Int], V.Vector a) where grow _ i c  = ([I.size c .. I.size c + i - 1], unchecked I.grow i c)
instance Growable q '[u, False] (V.Vector a)        (V.Vector a) where grow _ i c  = runST $ V.unsafeFreeze =<< flip MV.unsafeGrow i =<< V.unsafeThaw c

type instance ModsOf (V.Vector a) Expandable = ModsOf (V.Vector a) Growable
instance I.GrowableT q (V.Vector a) out => Expandable q mods (V.Vector a) out where expand spec = grow (rebaseSpecX spec) 1




-- === Concatenation ===
-- [+] Appendable
-- [+] Prependable
-- [+] Addable
-- [ ] Removable

type instance ModsOf (V.Vector a) Appendable  = '[Ixed ]
type instance ModsOf (V.Vector a) Prependable = '[Ixed ]
type instance ModsOf (V.Vector a) Addable     = '[Ixed ]

--class AppendableX    q m cont     el where appendx    :: InstModsX AppendableX      q m cont ->        el -> cont -> ElResult q cont el

type SimpleElResult q cont     el = MonoResultEl q cont el ~ cont
type IxedElResult   q cont idx el = MonoResultEl q cont el ~ (idx, cont)

instance (a ~ a', IxedElResult   q (V.Vector a) Int a) => AppendableX  q '[True ] (V.Vector a) a' where appendx _ a c = (V.length c' - 1, c') where c' = V.snoc c a
instance (a ~ a', SimpleElResult q (V.Vector a)     a) => AppendableX  q '[False] (V.Vector a) a' where appendx _ a c = V.snoc c a



instance a ~ a' => Appendable  q '[True ] (V.Vector a) a' (Int, V.Vector a) where append _ a c = (V.length c' - 1, c') where c' = V.snoc c a
instance a ~ a' => Appendable  q '[False] (V.Vector a) a'      (V.Vector a) where append _ a c = V.snoc c a

instance a ~ a' => Prependable q '[True ] (V.Vector a) a' (Int, V.Vector a) where prepend _ a c = (0, V.cons a c)
instance a ~ a' => Prependable q '[False] (V.Vector a) a'      (V.Vector a) where prepend _ a c = V.cons a c

instance (I.AppendableT q (V.Vector a) el out) => Addable q mods (V.Vector a) el out where add = append . rebaseSpecX



-- === Modification ===

-- [+] Indexable
-- [+] Insertable
-- [ ] Reservable
-- [ ] Releasable

type instance ModsOf (V.Vector a) Indexable   = '[Unchecked]
type instance ModsOf (V.Vector a) Insertable  = '[Unchecked]

instance (a ~ a', idx ~ Int) => Indexable   q '[False] (V.Vector a) idx a' where index  _ idx   c = (V.!)           c idx
instance (a ~ a', idx ~ Int) => Indexable   q '[True ] (V.Vector a) idx a' where index  _ idx   c = (V.unsafeIndex) c idx

instance (a ~ a', idx ~ Int) => Insertable  q '[False] (V.Vector a) Int a' (V.Vector a) where insert _ idx a c = (V.//)        c [(idx,a)]
instance (a ~ a', idx ~ Int) => Insertable  q '[True ] (V.Vector a) Int a' (V.Vector a) where insert _ idx a c = (V.unsafeUpd) c [(idx,a)]



-- === Indexing ===

-- [+] TracksElems
-- [+] TracksIxes
-- [-] TracksFreeIxes
-- [-] TracksUsedIxes

type instance ModsOf (V.Vector a) TracksElems = '[]
type instance ModsOf (V.Vector a) TracksIxes  = '[]

instance TracksElems q mods (V.Vector a) [a]   where elems   _   = V.toList
instance TracksIxes  q mods (V.Vector a) [Int] where indexes _ c = [0 .. I.size c -1]




---- missing instances ----

instance Default (V.Vector a) where def = mempty


