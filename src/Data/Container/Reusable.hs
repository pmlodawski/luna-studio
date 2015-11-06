{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE DeriveAnyClass       #-}

module Data.Container.Reusable where


import Prologue hiding (index, Indexable)

import           Data.Container.Class
import           Data.Container.Opts  (Query(..), ModsOf, ParamsOf)
import qualified Data.Container.Opts  as M
import           Data.Container.Proxy
import           Data.Layer
import           Data.List            ((\\))



----------------------
-- === Reusable === --
----------------------

data Reusable idx a = Reusable [idx] !a deriving (Show, Functor, Foldable, Traversable, Default, Monoid)
type Reusable'    a = Reusable (IndexOf a) a

type instance IndexOf      (Reusable idx a) = IndexOf (ContainerOf a)
type instance ContainerOf  (Reusable idx a) = Reusable idx a
type instance DataStoreOf  (Reusable idx a) = ContainerOf a

instance Monad m => IsContainerM  m (Reusable idx a) where fromContainerM = return
instance Monad m => HasContainerM m (Reusable idx a) where viewContainerM = return
                                                           setContainerM  = const . return

type instance       Unlayered  (Reusable idx a) = a
instance            Layered    (Reusable idx a) where layered = lens (\(Reusable _ a) -> a) (\(Reusable ixs _) a -> Reusable ixs a)
instance Monad m => LayeredM m (Reusable idx a)

instance      (IsContainer a, FromList (ContainerOf a)) 
           => FromList  (Reusable idx a) where fromList = Reusable mempty . fromContainer . fromList
type instance Item      (Reusable idx a) = Item (ContainerOf a)

_indexes :: Lens' (Reusable idx a) [idx]
_indexes = lens (\(Reusable ixs _) -> ixs) (\(Reusable _ a) ixs -> Reusable ixs a)



------------------------
-- === Instances === ---
------------------------

-- === Finite ===

-- [+] Measurable
-- [+] MinBounded
-- [+] MaxBounded

type instance ParamsOf MeasurableOp (Reusable idx a) = ParamsOf MeasurableOp (ContainerOf a)
type instance ModsOf   MeasurableOp (Reusable idx a) = ModsOf   MeasurableOp (ContainerOf a)

type instance ParamsOf MinBoundedOp (Reusable idx a) = ParamsOf MinBoundedOp (ContainerOf a)
type instance ModsOf   MinBoundedOp (Reusable idx a) = ModsOf   MinBoundedOp (ContainerOf a)

type instance ParamsOf MaxBoundedOp (Reusable idx a) = ParamsOf MaxBoundedOp (ContainerOf a)
type instance ModsOf   MaxBoundedOp (Reusable idx a) = ModsOf   MaxBoundedOp (ContainerOf a)

instance (MeasurableQM (GetOpts ms) (GetOpts ps) m     a)             => MeasurableQM_ ms ps m     (Reusable idx  a) where sizeM_     _ = sizeQM     (Query :: Query (GetOpts ms) (GetOpts ps)) . unlayer
instance (MinBoundedQM (GetOpts ms) (GetOpts ps) m idx a, idx ~ idx') => MinBoundedQM_ ms ps m idx (Reusable idx' a) where minBoundM_ _ = minBoundQM (Query :: Query (GetOpts ms) (GetOpts ps)) . unlayer
instance (MaxBoundedQM (GetOpts ms) (GetOpts ps) m idx a, idx ~ idx') => MaxBoundedQM_ ms ps m idx (Reusable idx' a) where maxBoundM_ _ = maxBoundQM (Query :: Query (GetOpts ms) (GetOpts ps)) . unlayer



-- === Construction ===

-- [+] Singleton
-- [+] Allocable
-- [+] Expandable
-- [+] Growable

type instance ParamsOf SingletonOp  (Reusable idx a) = ParamsOf SingletonOp  (ContainerOf a)
type instance ModsOf   SingletonOp  (Reusable idx a) = ModsOf   SingletonOp  (ContainerOf a)

type instance ParamsOf AllocableOp  (Reusable idx a) = ParamsOf AllocableOp  (ContainerOf a)
type instance ModsOf   AllocableOp  (Reusable idx a) = ModsOf   AllocableOp  (ContainerOf a)

type instance ParamsOf ExpandableOp (Reusable idx a) = ParamsOf ExpandableOp (ContainerOf a)
type instance ModsOf   ExpandableOp (Reusable idx a) = ModsOf   ExpandableOp (ContainerOf a)

type instance ParamsOf GrowableOp   (Reusable idx a) = ParamsOf GrowableOp   (ContainerOf a)
type instance ModsOf   GrowableOp   (Reusable idx a) = ModsOf   GrowableOp   (ContainerOf a)

instance ( SingletonQM (M.Ixed ': GetOpts ms) (GetOpts ps) m el a, idx ~ IndexOf (ContainerOf a)) => SingletonQM_ ms ps m el (Reusable idx a) where 
    singletonM_ _ el = do Res (ix,ds) r <- singletonQM (Query :: Query (M.Ixed ': GetOpts ms) (GetOpts ps)) el
                          return $ Res ds $ Reusable [ix] r

instance ( AllocableQM (M.Ixed ': GetOpts ms) (GetOpts ps) m a, idx ~ IndexOf (ContainerOf a)) => AllocableQM_ ms ps m (Reusable idx a) where 
    allocM_ _ i = do Res (ixs,ds) r <- allocQM (Query :: Query (M.Ixed ': GetOpts ms) (GetOpts ps)) i
                     return $ Res ds $ Reusable ixs r

instance ( ExpandableQM (M.Ixed ': GetOpts ms) (GetOpts ps) m a, idx ~ IndexOf (ContainerOf a)) => ExpandableQM_ ms ps m (Reusable idx a) where 
    expandM_ _ (Reusable ixs a) = do Res (ixs',ds) r <- expandQM (Query :: Query (M.Ixed ': GetOpts ms) (GetOpts ps)) a
                                     return $ Res ds $ Reusable (ixs <> ixs') r

instance ( GrowableQM (M.Ixed ': GetOpts ms) (GetOpts ps) m a, idx ~ IndexOf (ContainerOf a)) => GrowableQM_ ms ps m (Reusable idx a) where 
    growM_ _ i (Reusable ixs a) = do Res (ixs',ds) r <- growQM (Query :: Query (M.Ixed ': GetOpts ms) (GetOpts ps)) i a
                                     return $ Res ds $ Reusable (ixs <> ixs') r



-- === Modification ===
-- [+] Appendable
-- [+] Prependable
-- [+] Addable
-- [ ] Removable
-- [ ] Insertable
-- [+] Freeable

type instance ParamsOf AppendableOp   (Reusable idx a) = ParamsOf AppendableOp  (ContainerOf a)
type instance ModsOf   AppendableOp   (Reusable idx a) = ModsOf   AppendableOp  (ContainerOf a)

type instance ParamsOf PrependableOp  (Reusable idx a) = ParamsOf PrependableOp  (ContainerOf a)
type instance ModsOf   PrependableOp  (Reusable idx a) = ModsOf   PrependableOp  (ContainerOf a)

type instance ParamsOf AddableOp  (Reusable idx a) = '[]
type instance ModsOf   AddableOp  (Reusable idx a) = '[]

type instance ParamsOf FreeableOp  (Reusable idx a) = ParamsOf FreeableOp  (ContainerOf a)
type instance ModsOf   FreeableOp  (Reusable idx a) = ModsOf   FreeableOp  (ContainerOf a)


instance (AppendableQM  (GetOpts ms) (GetOpts ps) m el a)           => AppendableQM_  ms ps m el   (Reusable idx  a) where appendM_  _      = nested layered . appendQM  (Query :: Query (GetOpts ms) (GetOpts ps))
instance (PrependableQM (GetOpts ms) (GetOpts ps) m el a)           => PrependableQM_ ms ps m el   (Reusable idx  a) where prependM_ _      = nested layered . prependQM (Query :: Query (GetOpts ms) (GetOpts ps))
instance (InsertableM m idx el a, ExpandableM m (Reusable idx a))   => AddableQM_    '[] ps m el   (Reusable idx  a) where addM_     q el t = case view _indexes t of
                                                                                                                               (x:xs) -> fmap2 (Reusable xs) $ insertM' x el $ unlayer t
                                                                                                                               []     -> addM_ q el =<< expandM t
instance (FreeableQM (GetOpts ms) (GetOpts ps) m idx a, idx ~ idx') => FreeableQM_    ms ps m idx  (Reusable idx' a) where freeM_ _ idx     = fmap2 (_indexes %~ (idx:)) . nested layered (freeQM (Query :: Query (GetOpts ms) (GetOpts ps)) idx)



---- === Indexing ===

-- [+] Indexable
-- [+] TracksFreeIxes
-- [+] TracksUsedIxes
-- [+] TracksIxes
-- [+] TracksElems

type instance ParamsOf IndexableOp      (Reusable idx a) = ParamsOf IndexableOp      (ContainerOf a)
type instance ModsOf   IndexableOp      (Reusable idx a) = ModsOf   IndexableOp      (ContainerOf a)

type instance ParamsOf TracksIxesOp     (Reusable idx a) = ParamsOf TracksIxesOp (ContainerOf a)
type instance ModsOf   TracksIxesOp     (Reusable idx a) = ModsOf   TracksIxesOp (ContainerOf a)

type instance ParamsOf TracksFreeIxesOp (Reusable idx a) = '[]
type instance ModsOf   TracksFreeIxesOp (Reusable idx a) = '[]

type instance ParamsOf TracksUsedIxesOp (Reusable idx a) = '[]
type instance ModsOf   TracksUsedIxesOp (Reusable idx a) = '[]

type instance ParamsOf TracksElemsOp    (Reusable idx a) = '[]
type instance ModsOf   TracksElemsOp    (Reusable idx a) = '[]


instance (IndexableQM      (GetOpts ms) (GetOpts ps) m idx el a, idx ~ idx') => IndexableQM_       ms ps m idx el (Reusable idx' a) where indexM_     _ idx   = indexQM    (Query :: Query (GetOpts ms) (GetOpts ps)) idx . unlayer
instance (TracksIxesQM     (GetOpts ms) (GetOpts ps) m idx    a, idx ~ idx') => TracksIxesQM_      ms ps m idx    (Reusable idx' a) where ixesM_      _       = ixesQM     (Query :: Query (GetOpts ms) (GetOpts ps))     . unlayer
instance (Monad m, idx ~ idx')                                               => TracksFreeIxesQM_ '[] ps m idx    (Reusable idx' a) where freeIxesM_  _       = return . Res () . view _indexes

instance (TracksIxes idx (Reusable idx a)
         , TracksFreeIxes idx (Reusable idx a), idx ~ idx', Monad m, Eq idx) => TracksUsedIxesQM_ '[] ps m idx    (Reusable idx' a) where usedIxesM_  _     t = return $ Res () $ ixes t \\ freeIxes t

instance ( TracksUsedIxes  idx    (Reusable idx a)
         , Indexable       idx el (Reusable idx a)
         , Monad           m
         , Tup2RTup el ~ (el, ()) -- TODO [WD]: Remove me, add smart constraints to super-class
         ) => TracksElemsQM_     '[] ps m     el (Reusable idx a) where elemsM_     _   t = return $ Res () $ fmap (flip index t) (usedIxes t :: [idx]) where
