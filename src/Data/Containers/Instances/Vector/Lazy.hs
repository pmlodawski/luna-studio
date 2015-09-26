{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

module Data.Containers.Instances.Vector.Lazy where

import           Prologue hiding (Indexable, index, Bounded, Ixed, switch, Simple, simple)
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
import           Data.Maybe (fromJust)
import GHC.Prim

import Data.TypeLevel.List (In)

-- === TF Instances ===

type instance ContainerOf   (V.Vector a) = V.Vector a
instance      IsContainer   (V.Vector a)              where fromContainer = id
instance      HasContainer  (V.Vector a)              where container     = id

type instance ElementOf        (V.Vector a) = a
type instance ElementByIx  idx (V.Vector a) = a
type instance IndexOf      el  (V.Vector a) = Int


type instance DataStoreOf  (V.Vector a) = V.Vector a
instance      HasDataStore (V.Vector a) where dataStore     = id
instance      IsDataStore  (V.Vector a) where fromDataStore = id


-- === Finite ===

-- [+] Measurable
-- [+] MinBounded
-- [+] MaxBounded

type instance ModsOf MeasurableQSM (V.Vector a) = '[]
type instance ModsOf MinIndexedQSM (V.Vector a) = '[]
type instance ModsOf MaxIndexedQSM (V.Vector a) = '[]

instance Monad m => MeasurableQSM (V.Vector a) m q s where sizeQSM     _ _   = simpleM . V.length
instance Monad m => MinIndexedQSM (V.Vector a) m q s where minIndexQSM _ _ _ = simpleM 0
instance Monad m => MaxIndexedQSM (V.Vector a) m q s where maxIndexQSM _ _   = simpleM . pred . size


-- === Construction ===

-- [+] Singleton
-- [+] Allocable
-- [+] Expandable
-- [+] Growable


type instance          ModsOf SingletonQSM    (V.Vector a)   = '[Ixed ]
instance (Monad m, a ~ a') => SingletonQSM a' (V.Vector a) m q '[False] where singletonQSM _ _ = simpleM . V.singleton
instance (Monad m, a ~ a') => SingletonQSM a' (V.Vector a) m q '[True ] where singletonQSM _ _ = resM 0  . singleton

type instance          ModsOf AllocableQSM    (V.Vector a)   = '[Ixed ]
instance           Monad m => AllocableQSM    (V.Vector a) m q '[False] where allocQSM _ _ i   = simpleM $ runST $ V.unsafeFreeze =<< MV.unsafeNew i
instance           Monad m => AllocableQSM    (V.Vector a) m q '[True ] where allocQSM _ _ i   = resM [0..i-1] $ alloc i

type instance          ModsOf ExpandableQSM   (V.Vector a)   = ModsOf GrowableQSM (V.Vector a)
instance          (Monad m, GrowableQM q m (V.Vector a), TransCheck q GrowableInfo ExpandableInfo (V.Vector a))
                           => ExpandableQSM   (V.Vector a) m q s        where expandQSM _ _    = queried (Proxy :: Proxy q) growM' 1

type instance          ModsOf GrowableQSM     (V.Vector a)   = '[Ixed ]
instance          Monad m  => GrowableQSM     (V.Vector a) m q '[False] where growQSM _ _ i v  = simpleM $ runST $ V.unsafeThaw v >>= flip MV.unsafeGrow i >>= V.unsafeFreeze
instance          Monad m  => GrowableQSM     (V.Vector a) m q '[True ] where growQSM _ _ i v  = resM [size v .. size v + i - 1] $ grow i v



-- === Modification ===
-- [+] Appendable
-- [ ] Prependable
-- [ ] Addable
-- [ ] Removable
-- [+] Insertable


type instance            ModsOf AppendableQSM    (V.Vector a)   = '[Ixed ]
instance   (a ~ a', Monad m) => AppendableQSM a' (V.Vector a) m q '[False] where appendQSM _ _ el v  = simpleM       $ V.snoc v el
instance   (a ~ a', Monad m) => AppendableQSM a' (V.Vector a) m q '[True ] where appendQSM _ _ el v  = resM (size v) $ append el v

type instance                       ModsOf InsertableQSM        (V.Vector a)   = '[Ixed ]
instance   (a ~ a', idx ~ Int, Monad m) => InsertableQSM idx a' (V.Vector a) m q '[False] where insertQSM _ _ idx el v = simpleM  $ (V.//) v [(idx,el)]
instance   (a ~ a', idx ~ Int, Monad m) => InsertableQSM idx a' (V.Vector a) m q '[True ] where insertQSM _ _ idx      = resM idx .: insert idx


---- === Indexing ===

-- [+] Indexable
-- [ ] TracksElems
-- [ ] TracksIxes
-- [ ] TracksFreeIxes
-- [ ] TracksUsedIxes


type instance                                                 ModsOf IndexableQSM        (V.Vector a)   = '[Unchecked, Try]
instance   (a ~ a', idx ~ Int, Cond unchecked, Cond try, Monad m) => IndexableQSM idx a' (V.Vector a) m q '[unchecked, try] where indexQSM _ _ idx v = simple' <$> checkedBoundsIfM (Proxy :: Proxy unchecked) (Proxy :: Proxy try) idx v (V.unsafeIndex v idx)


--type instance                                                 ModsOf TrackIxesQSM        (V.Vector a)   = '[Unchecked, Try]
--instance   (a ~ a', idx ~ Int, Cond unchecked, Cond try, Monad m) => TrackIxesQSM idx a' (V.Vector a) m q '[unchecked, try] where indexQSM _ _ idx v = simple' <$> checkedBoundsIfM (Proxy :: Proxy unchecked) (Proxy :: Proxy try) idx v (V.unsafeIndex v idx)







simple' = Result ()
res     = Result . (,())
resM    = return .: res
simpleM = return . simple'

failT p = ifT p fail error

checkBounds i v l r = if i > max || i < 0 then l ("index " <> show i <> " out of bounds x[0," <> show max <> "]") else r where max = size v - 1
checkBoundsM p idx v = checkBounds idx v (const . failT p) return
checkedBoundsIfM unchecked try idx v = checkedIfM unchecked (checkBoundsM try idx v)

checkedIfM p = ifT p return


checkedIdxIf  cond i f = ifT cond f $ if i < 0 then error "negative index" else f
checkedSizeIf cond i f = ifT cond f $ if i < 0 then error "negative sizeQSM"  else f



---- missing instances ----

instance Default (V.Vector a) where def = mempty


