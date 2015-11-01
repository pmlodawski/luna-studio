{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Data.Container.Weak where

import Prologue              hiding (Indexable, index, Bounded, Ixed)
import Data.Container.Class
import Data.Typeable
import qualified Data.Container.Interface as I
import           Data.Container.Poly {- x -}
import qualified Data.Container.Mods as Mods

-- import Data.Container.Parametrized

import qualified System.Mem.Weak as Mem
import System.IO.Unsafe (unsafePerformIO)
import           Data.Layer
import           Data.Maybe (catMaybes)

-- Types


data Weak     c a = Weak (Maybe (HomoIndexOf c -> IO ())) (WeakData c a)
type WeakData c a = Unlayered (Weak c a)


type instance Unlayered (Weak c a) = c (Mem.Weak a)
instance      Layered   (Weak c a) where layered = lens (\(Weak _ a) -> a) (\(Weak f _) a -> Weak f a)


class Finalizer a where
    finalizer :: Lens' a (Maybe (IndexOf' a -> IO ()))

instance Finalizer (Weak c a) where finalizer = lens (\(Weak f d) -> f) (\(Weak _ d) f -> Weak f d)

instance {-# OVERLAPPABLE #-} (IndexOf (ElementOf (Unlayered a)) (Unlayered a) ~ IndexOf (ElementOf a) a, Finalizer (Unlayered a), Layered a)
                           => Finalizer a where finalizer = layered . finalizer


--

type instance ContainerOf (Weak c a) = Weak c a

instance IsContainer  (Weak c a) where fromContainer = id
instance HasContainer (Weak c a) where container     = id

type instance ElementOf        (Weak c a) = a
type instance ElementByIx  idx (Weak c a) = ElementByIx idx (c a)
type instance IndexOf      a   (Weak c a) = HomoIndexOf c

type instance DataStoreOf (Weak c a) = DataStoreOf (WeakData c a)

instance Default (WeakData c a) => Default (Weak c a) where def = Weak def def

type instance HomoIndexOf (Weak c) = HomoIndexOf c


-- Instances

instance Monoid (WeakData c a) => Monoid (Weak c a) where
    mempty                            = Weak Nothing mempty
    mappend (Weak mf a) (Weak mf' a') = Weak Nothing (a <> a') -- FIXME: Nothing -> merge actions

instance (Functor c, Show (c (Maybe a))) => Show (Weak c a) where
    showsPrec d (Weak mf c) = showParen (d > app_prec) $
            showString "Weak " . showsPrec (app_prec+1) (fmap (unsafePerformIO . Mem.deRefWeak) c)
         where app_prec = 10
    {-# NOINLINE showsPrec #-}


type instance Item (Weak c a) = Item (c a)
instance (Functor c, FromList (c a)) => FromList (Weak c a) where fromList = Weak Nothing . fmap (unsafePerformIO . flip Mem.mkWeakPtr Nothing) . fromList
                                                                  {-# NOINLINE fromList #-}




---- === Finite ===

---- [+] Measurable
---- [+] MinBounded
---- [+] MaxBounded


type instance ModsOf MeasurableQSM (Weak c a) = ModsOf MeasurableQSM (WeakData c a)
--type instance ModsOf MinIndexedQSM (Weak c a) = ModsOf MinIndexedQSM a
--type instance ModsOf MaxIndexedQSM (Weak c a) = ModsOf MaxIndexedQSM a

instance MeasurableQM q m (WeakData c a) => MeasurableQSM (Weak c a) m q s where sizeQSM     _ _ = queried (Proxy :: Proxy q) sizeM' . unlayer
--instance MinIndexedQM q m a => MinIndexedQSM (Weak c a) m q s where minIndexQSM _ _ = queried (Proxy :: Proxy q) minIndexM' . unwrap
--instance MaxIndexedQM q m a => MaxIndexedQSM (Weak c a) m q s where maxIndexQSM _ _ = queried (Proxy :: Proxy q) maxIndexM' . unwrap

weaked = lens (\(Weak mf a) -> a) (\(Weak mf _) a -> Weak mf a)

nestedWeaked = nested weaked

---- === Construction ===

---- [+] Singleton
---- [ ] Allocable
---- [+] Expandable
---- [+] Growable

type instance ModsOf SingletonQSM  (Weak c a) = ModsOf SingletonQSM  (WeakData c a)
type instance ModsOf AllocableQSM  (Weak c a) = ModsOf AllocableQSM  (WeakData c a)
type instance ModsOf ExpandableQSM (Weak c a) = ModsOf ExpandableQSM (WeakData c a)
type instance ModsOf GrowableQSM   (Weak c a) = ModsOf GrowableQSM   (WeakData c a)

instance (MonadIO m, SingletonQM (Mem.Weak el) q m t, t ~ WeakData c a, cls ~ SingletonQSM,
    EqInfoQueries (SingletonInfo el (DataStoreOf t)) (SingletonInfo (Mem.Weak el) (DataStoreOf t)) (Mods.FilterMutable q) ) => SingletonQSM el (Weak c a) m q s where
    singletonQSM _ _ el = do
        ptr <- liftIO $ Mem.mkWeakPtr el Nothing
        (fmap . fmap) (Weak Nothing) $ queried (Proxy :: Proxy q) singletonM' ptr

instance (AllocableQM  q m (WeakData c a)) => AllocableQSM  (Weak c a) m q s where allocQSM  _ _  = (fmap . fmap) (Weak Nothing) . queried (Proxy :: Proxy q) allocM'
instance (ExpandableQM q m (WeakData c a)) => ExpandableQSM (Weak c a) m q s where expandQSM _ _  = nestedWeaked  $ queried (Proxy :: Proxy q) expandM'
instance (GrowableQM   q m (WeakData c a)) => GrowableQSM   (Weak c a) m q s where growQSM   _ _  = nestedWeaked  . queried (Proxy :: Proxy q) growM'



---- === Modification ===

---- [+] Appendable
---- [ ] Prependable
---- [ ] Addable
---- [ ] Removable
---- [+] Insertable

type instance ModsOf AppendableQSM (Weak c a) = ModsOf AppendableQSM (WeakData c a)
type instance ModsOf FreeableQSM   (Weak c a) = ModsOf FreeableQSM   (WeakData c a)
type instance ModsOf InsertableQSM (Weak c a) = ModsOf InsertableQSM (WeakData c a)


instance (MonadIO m, MonadFix m, AppendableQM wel (Mods.Ixed ': q) m t
         , t    ~ WeakData c a
         , wel  ~ Mem.Weak el
         , ds   ~ DataStoreOf t
         , info ~ AppendableInfo wel ds
         , idx  ~ ModData Mods.Ixed info
         , idx  ~ HomoIndexOf c
         , EqInfoQueries (AppendableInfo el ds) info (Mods.FilterMutable q)
         ) => AppendableQSM el (Weak c a) m q s where
    appendQSM _ _ el (Weak f c) = mdo
        ptr <- liftIO $ Mem.mkWeakPtr el (($ ixs) <$> f)
        (ixs, r) <- splitResData <$> nestedWeaked ( (ixed . queried (Proxy :: Proxy q)) appendM' ptr) (Weak f c)
        return r

instance (MonadIO m, MonadFix m, InsertableQM idx wel (Mods.Ixed ': q) m t
         , t    ~ WeakData c a
         , wel  ~ Mem.Weak el
         , ds   ~ DataStoreOf t
         , info ~ InsertableInfo idx wel ds
         , idx  ~ ModData Mods.Ixed info
         , idx  ~ HomoIndexOf c
         , EqInfoQueries (InsertableInfo idx el ds) info (Mods.FilterMutable q)
         ) => InsertableQSM idx el (Weak c a) m q s where
    insertQSM _ _ idx el (Weak f c) = mdo
        ptr <- liftIO $ Mem.mkWeakPtr el (($ ixs) <$> f)
        (ixs, r) <- splitResData <$> nestedWeaked ( (ixed . queried (Proxy :: Proxy q)) insertM' idx ptr) (Weak f c)
        return r

instance FreeableQM idx q m (WeakData c a) => FreeableQSM idx (Weak c a) m q s where freeQSM   _ _  = nestedWeaked . queried (Proxy :: Proxy q) freeM'


------ === Indexing ===

---- [+] Indexable
---- [ ] TracksElems
---- [ ] TracksIxes
---- [+] TracksFreeIxes
---- [ ] TracksUsedIxes


--type instance ModsOf IndexableQSM      (Weak c a) = ModsOf IndexableQSM a
--type instance ModsOf TracksFreeIxesQSM (Weak c a) = '[]
type instance ModsOf TracksIxesQSM  (Weak c a) = ModsOf TracksIxesQSM  (WeakData c a)
type instance ModsOf TracksElemsQSM (Weak c a) = ModsOf TracksElemsQSM (WeakData c a)

instance ( t    ~ WeakData c a
         , ds   ~ DataStoreOf t
         , wel  ~ Mem.Weak el
         , info ~ TracksElemsInfo wel ds
         , TracksElemsQM (Mem.Weak el) q m (WeakData c a)
         , EqInfoQueries (TracksElemsInfo el ds) info (Mods.FilterMutable q)
         ) => TracksElemsQSM   el (Weak c a) m q s where elemsQSM _ _   = (fmap . fmap) (catMaybes . fmap (unsafePerformIO . Mem.deRefWeak)) . queried (Proxy :: Proxy q) elemsM' . unlayer


instance   TracksIxesQM  idx q m (WeakData c a) => TracksIxesQSM  idx (Weak c a) m q s where ixesQSM  _ _     = queried (Proxy :: Proxy q) ixesM'      . unlayer


--type instance ModsOf TracksElemsQSM (HResizable l a) = ModsOf TracksElemsQSM a

--instance   IndexableQM   idx el q m a => IndexableQSM   idx el (HResizable l a) m q s where indexQSM _ _ idx = queried (Proxy :: Proxy q) indexM' idx . unlayer
--instance   TracksIxesQM  idx    q m a => TracksIxesQSM  idx    (HResizable l a) m q s where ixesQSM  _ _     = queried (Proxy :: Proxy q) ixesM'      . unlayer