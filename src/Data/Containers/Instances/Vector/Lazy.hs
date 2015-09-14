{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE PolyKinds #-}

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


-- === TF Instances ===

type instance ContainerOf (V.Vector a) = V.Vector a
instance HasContainer (V.Vector a) (V.Vector a) where container = id
instance IsContainer (V.Vector a) where fromContainer = id
instance HasContainer2 (V.Vector a) where container2 = id

type instance ElementOf        (V.Vector a) = a
type instance ElementByIx  idx (V.Vector a) = a
type instance IndexOf      el  (V.Vector a) = Int



-- === Finite ===

-- [+] Measurable
-- [+] MinBounded
-- [+] MaxBounded

--type instance ModsOf (V.Vector a) Measurable  = '[]
--type instance ModsOf (V.Vector a) MinIndexed  = '[]
--type instance ModsOf (V.Vector a) MaxIndexed  = '[]

--instance      Measurable  q mods (V.Vector a) Int where sizeQSM _ = V.length
--instance      MinIndexed  q mods (V.Vector a) Int where minIndexQSM _ _ = 0
--instance      MaxIndexed  q mods (V.Vector a) Int where maxIndexQSM _ c = I.sizeQSM c - 1





--type instance ModsOf (V.Vector a) Singleton   = '[]
--instance                a ~ a' => Singleton   q mods         (V.Vector a) a' where singletonQSM _ = V.singletonQSM

---- FIXME[wd]: We should check if the Unchecked flag is on and check for the negative number
--type instance ModsOf (V.Vector a) Allocable   = '[Unchecked]
--instance                          Allocable   q mods         (V.Vector a)    where allocQSM _ i = runST $ V.unsafeFreeze =<< MV.unsafeNew i

---- FIXME[wd]: We should check if the Unchecked flag is on and check for the negative number
--type instance ModsOf (V.Vector a) Growable = '[Unchecked, Ixed]
--instance Growable q '[u, True ] (V.Vector a) ([Int], V.Vector a) where growQSM _ i c  = ([I.sizeQSM c .. I.sizeQSM c + i - 1], unchecked I.growQSM i c)
--instance Growable q '[u, False] (V.Vector a)        (V.Vector a) where growQSM _ i c  = runST $ V.unsafeFreeze =<< flip MV.unsafeGrow i =<< V.unsafeThaw c

--type instance ModsOf (V.Vector a) Expandable = ModsOf (V.Vector a) Growable
--instance I.GrowableT q (V.Vector a) out => Expandable q mods (V.Vector a) out where expandQSM spec = growQSM (rebaseSpecX spec) 1

----type CheckResult
----class ExpandableF    q s cont m        where expandF    :: Query q s -> Info ExpandableF cont NA NA ->               cont -> m (ResultF q (Info ExpandableF cont NA NA) cont)



----instance ResultF q (ExpandableInfo (V.Vector a)) ~ Simple => ExpandableF q s (V.Vector a) m where expandF _ _ v = return (Simple v)
----instance SimpleResult q (RawInfo ExpandableF (V.Vector a)) => ExpandableF q s (V.Vector a) m where expandF _ _ v = return (Simple v)


----instance SimpleRawResult ExpandableF q (V.Vector a) => ExpandableF q s (V.Vector a) m where expandF _ _ v = simple $ runST $ V.unsafeFreeze =<< flip MV.unsafeGrow 1 =<< V.unsafeThaw v


-- === Finite ===

-- [+] Measurable
-- [+] MinBounded
-- [+] MaxBounded

type instance ModsOf MeasurableQSM (V.Vector a) = '[]
type instance ModsOf MinIndexedQSM (V.Vector a) = '[]
type instance ModsOf MaxIndexedQSM (V.Vector a) = '[]

instance Monad m  => MeasurableQSM (V.Vector a) m q s where sizeQSM     _ _   = simple . V.length
instance Monad m  => MinIndexedQSM (V.Vector a) m q s where minIndexQSM _ _ _ = simple 0
instance Monad m  => MaxIndexedQSM (V.Vector a) m q s where maxIndexQSM _ _   = (fmap.fmap) pred . sizeM'


-- === Construction ===

-- [+] Singleton
-- [+] Allocable
-- [+] Expandable
-- [+] Growable

type instance          ModsOf SingletonQSM   (V.Vector a)   = '[Ixed ]
instance          Monad m  => SingletonQSM a (V.Vector a) m q '[False]            where singletonQSM _ _    = simple . V.singleton
instance          Monad m  => SingletonQSM a (V.Vector a) m q '[True ]            where singletonQSM _ _ el = return (0, V.singleton el)

type instance          ModsOf AllocableQSM   (V.Vector a)   = '[Ixed , Unchecked]
instance (Monad m, Cond u) => AllocableQSM   (V.Vector a) m q '[False, u        ] where allocQSM _ _ i = checkedSizeIf (Proxy :: Proxy u) i $ simple $ runST $ V.unsafeFreeze =<< MV.unsafeNew i
instance (Monad m, Cond u) => AllocableQSM   (V.Vector a) m q '[True , u        ] where allocQSM _ _ i = checkedSizeIf (Proxy :: Proxy u) i $ ([0 .. i - 1],) <$> unchecked allocM2 i

type instance          ModsOf ExpandableQSM  (V.Vector a)   = '[Ixed ]
instance          Monad m  => ExpandableQSM  (V.Vector a) m q '[False]            where expandQSM _ _ v = simple $ runST $ V.unsafeThaw v >>= flip MV.unsafeGrow 1 >>= V.unsafeFreeze
instance          Monad m  => ExpandableQSM  (V.Vector a) m q '[True ]            where expandQSM _ _ v = (,) <$> ((:[]) <$> sizeM v) <*> expandM2 v

type instance          ModsOf GrowableQSM    (V.Vector a)   = '[Ixed , Unchecked]
instance (Monad m, Cond u) => GrowableQSM    (V.Vector a) m q '[False, u        ] where growQSM _ _ i v = checkedIdxIf (Proxy :: Proxy u) i $ simple $ runST $ V.unsafeThaw v >>= flip MV.unsafeGrow i >>= V.unsafeFreeze
instance (Monad m, Cond u) => GrowableQSM    (V.Vector a) m q '[True , u        ] where growQSM _ _ i v = checkedIdxIf (Proxy :: Proxy u) i $ (\s c -> ([s .. s + i - 1], c)) <$> sizeM v <*> unchecked growM2 i v


-- === Concatenation ===
-- [+] Appendable
-- [+] Prependable
-- [+] Addable
-- [+] Removable

type instance                ModsOf AppendableQSM     (V.Vector a)   = '[Ixed ]
instance (a ~ a', Monad m)       => AppendableQSM  a' (V.Vector a) m q '[False]        where appendQSM  _ _ el v = simple $ V.snoc v el
instance (a ~ a', Monad m)       => AppendableQSM  a' (V.Vector a) m q '[True ]        where appendQSM  _ _ el v = (,) <$> sizeM v <*> appendM2 el v

type instance                ModsOf PrependableQSM    (V.Vector a)   = '[Ixed ]
instance (a ~ a', Monad m)       => PrependableQSM a' (V.Vector a) m q '[False]        where prependQSM _ _ el v = simple $ V.cons el v
instance (a ~ a', Monad m)       => PrependableQSM a' (V.Vector a) m q '[True ]        where prependQSM _ _ el v = (0,) <$> prependM2 el v

type instance                ModsOf AddableQSM        (V.Vector a)   = '[Ixed ]
instance (a ~ a', Monad m)       => AddableQSM     a' (V.Vector a) m q '[False]        where addQSM     _ _ el v = simple $ V.snoc v el
instance (a ~ a', Monad m)       => AddableQSM     a' (V.Vector a) m q '[True ]        where addQSM     _ _ el v = (,) <$> sizeM v <*> addM2 el v

type instance                ModsOf RemovableQSM      (V.Vector a)   = '[Try  , Ixed ]
instance (a ~ a', Eq a, Monad m) => RemovableQSM   a' (V.Vector a) m q '[False, False] where removeQSM  _ _ el v = Simple . maybe v id <$> try removeM2 el v
instance (a ~ a', Eq a, Monad m) => RemovableQSM   a' (V.Vector a) m q '[True , False] where removeQSM  _ _ el v = Maybed . fmap (Simple . snd) <$> (try . ixed) removeM2 el v
instance (a ~ a', Eq a, Monad m) => RemovableQSM   a' (V.Vector a) m q '[True , True ] where removeQSM  _ _ el v = maybed $ flip fmap (V.findIndex (== el) v)
                                                                                                                          $ \i -> let (l,r) = V.splitAt i v in (i,) $ l <> V.unsafeDrop 1 r

--instance          Monad m  => Removable a (V.Vector a) m q '[True ]               where addQSM _ _ el v = do s <- sizeM v
                                                                                                        --(s,) <$> addM2 el v


--instance          Monad m  => Appendable  (V.Vector a) m q '[True ]            where appendQSM _ _ v = (,) <$> ((:[]) <$> sizeM v) <*> expandM2 v

--class Appendable            el cont m q s where appendQSM    :: info ~ AppendableInfo el cont => Query q s -> info -> el  -> cont -> m (ResultBySel info s cont)

maybed = return . Maybed

checkedIdxIf  cond i f = ifT cond f $ if i < 0 then error "negative index" else f
checkedSizeIf cond i f = ifT cond f $ if i < 0 then error "negative sizeQSM"  else f


--instance Monad m  => Growable (V.Vector a) m q '[True ] where growQSM _ _ i v = (,) <$> ((:[]) <$> sizeM v) <*> expandM2 v

--class Growable                 cont m q s where growQSM      :: info ~ GrowableInfo      cont => Query q s -> info -> Int -> cont -> m (ResultBySel info s cont)


--instance Monad m  => Allocable a (V.Vector a) m q '[True ] where allocQSM _ _ el = return (0, V.allocQSM el)

--class Allocable             el cont m q s where allocQSM     :: info ~ AllocableInfo     cont => Query q s -> info -> Int        -> m (ResultBySel info s cont)


--instance Monad m  => Singleton (V.Vector a) m q '[True ] where singletonQSM _ _ el = (,) <$> sizeM v <*> expandM2 v


--class Singleton             el cont m q s where singletonQSM :: info ~ SingletonInfo  el cont => Query q s -> info -> el         -> m (ResultBySel info s cont)


--class MinIndexed               cont m q s where minIndexQSM  :: info ~ MinIndexedInfo    cont => Query q s -> info ->       cont -> m (ResultBySel info s (IndexOf' cont))


--instance Monad m  => Measurable (V.Vector a) m q '[True ] where sizeQSM _ _ v = (0,) <$> expandM v

--class Measurable               cont m q s where sizeQSM      :: info ~ MeasurableInfo    cont => Query q s -> info ->       cont -> m (ResultBySel info s Int)


--type instance ModsOf Appendable (V.Vector a)   = '[Ixed ]
--instance Monad m  => Appendable (V.Vector a) m q '[False] where expandQSM _ _ el v = simple $ runST $ V.unsafeFreeze =<< flip MV.unsafeGrow 1 =<< V.unsafeThaw v

--class Expandable               cont m q s where expandQSM    :: info ~ ExpandableInfo    cont => Query q s -> info ->       cont -> m (ResultBySel info s cont)
--class Appendable            el cont m q s where appendQSM    :: info ~ AppendableInfo el cont => Query q s -> info -> el -> cont -> m (ResultBySel info s cont)
--class Singleton             el cont m q s where singletonQSM :: info ~ SingletonInfo  el cont => Query q s -> info -> el         -> m (ResultBySel info s cont)


-- === Concatenation ===
-- [+] Appendable
-- [+] Prependable
-- [+] Addable
-- [ ] Removable

--type instance ModsOf (V.Vector a) Appendable  = '[Ixed ]
--type instance ModsOf (V.Vector a) Prependable = '[Ixed ]
--type instance ModsOf (V.Vector a) Addable     = '[Ixed ]

----class AppendableX    q m cont     el where appendx    :: InstModsX AppendableX      q m cont ->        el -> cont -> ElResult q cont el

--type SimpleElResult q cont     el = MonoResultEl q cont el ~ cont
--type IxedElResult   q cont idx el = MonoResultEl q cont el ~ (idx, cont)

--instance (a ~ a', IxedElResult   q (V.Vector a) Int a) => AppendableX  q '[True ] (V.Vector a) a' where appendx _ a c = (V.length c' - 1, c') where c' = V.snoc c a
--instance (a ~ a', SimpleElResult q (V.Vector a)     a) => AppendableX  q '[False] (V.Vector a) a' where appendx _ a c = V.snoc c a



--instance a ~ a' => Appendable  q '[True ] (V.Vector a) a' (Int, V.Vector a) where appendQSM _ a c = (V.length c' - 1, c') where c' = V.snoc c a
--instance a ~ a' => Appendable  q '[False] (V.Vector a) a'      (V.Vector a) where appendQSM _ a c = V.snoc c a

--instance a ~ a' => Prependable q '[True ] (V.Vector a) a' (Int, V.Vector a) where prependQSM _ a c = (0, V.cons a c)
--instance a ~ a' => Prependable q '[False] (V.Vector a) a'      (V.Vector a) where prependQSM _ a c = V.cons a c

--instance (I.AppendableT q (V.Vector a) el out) => Addable q mods (V.Vector a) el out where addQSM = appendQSM . rebaseSpecX



-- === Modification ===

-- [+] Indexable
-- [+] Insertable
-- [ ] Reservable
-- [ ] Releasable

--type instance ModsOf (V.Vector a) Indexable   = '[Unchecked]
--type instance ModsOf (V.Vector a) Insertable  = '[Unchecked]

--instance (a ~ a', idx ~ Int) => Indexable   q '[False] (V.Vector a) idx a' where index  _ idx   c = (V.!)           c idx
--instance (a ~ a', idx ~ Int) => Indexable   q '[True ] (V.Vector a) idx a' where index  _ idx   c = (V.unsafeIndex) c idx

--instance (a ~ a', idx ~ Int) => Insertable  q '[False] (V.Vector a) Int a' (V.Vector a) where insert _ idx a c = (V.//)        c [(idx,a)]
--instance (a ~ a', idx ~ Int) => Insertable  q '[True ] (V.Vector a) Int a' (V.Vector a) where insert _ idx a c = (V.unsafeUpd) c [(idx,a)]



-- === Indexing ===

-- [+] TracksEleQSM
-- [+] TracksIxes
-- [-] TracksFreeIxes
-- [-] TracksUsedIxes

--type instance ModsOf TracksEleQSM (V.Vector a) = '[]
--type instance ModsOf TracksIxes  (V.Vector a) = '[]

--instance TracksEleQSM q mods (V.Vector a) [a]   where eleQSM   _   = V.toList
--instance TracksIxes  q mods (V.Vector a) [Int] where indexes _ c = [0 .. I.sizeQSM c -1]




---- missing instances ----

instance Default (V.Vector a) where def = mempty


