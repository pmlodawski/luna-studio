{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}


{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

module Data.Container.Instances.Vector.Lazy where

import           Prologue hiding (Indexable, index, Bounded, switch)
import           Data.Container.Class

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
import qualified Data.Container.Interface as I
import           Data.Container.Poly {- x -}
import           Data.Maybe (fromJust)
import GHC.Prim

import qualified Data.Container.Mods as M
import Data.Layer
import Data.TypeLevel.List (In)

-- === TF Instances ===

--type instance ContainerOf   (V.Vector a) = V.Vector a
instance      IsContainer   (V.Vector a)              where fromContainer = id
--instance      HasContainer  (V.Vector a)              where container     = id

type instance ElementOf        (V.Vector a) = a
type instance ElementByIx  idx (V.Vector a) = a
type instance IndexOf      el  (V.Vector a) = Int


type instance DataStoreOf  (V.Vector a) = V.Vector a
instance      HasDataStore (V.Vector a) where dataStore     = id
instance      IsDataStore  (V.Vector a) where fromDataStore = id

type instance HomoIndexOf V.Vector = Int


-- === Finite ===

-- [+] Measurable
-- [+] MinBounded
-- [+] MaxBounded

type instance ModsOf MeasurableQSM (V.Vector a) = '[]
type instance ModsOf MinIndexedQSM (V.Vector a) = '[]
type instance ModsOf MaxIndexedQSM (V.Vector a) = '[]

instance Monad m => MeasurableQSM (V.Vector a) m q s where sizeQSM     _ _   = simpleM . V.length
instance (Monad m, idx ~ Int) => MinIndexedQSM idx (V.Vector a) m q s where minIndexQSM _ _ _ = simpleM 0
instance (Monad m, idx ~ Int) => MaxIndexedQSM idx (V.Vector a) m q s where maxIndexQSM _ _   = simpleM . pred . size


-- === Construction ===

-- [+] Singleton
-- [+] Allocable
-- [+] Expandable
-- [+] Growable


type instance          ModsOf SingletonQSM    (V.Vector a)   = '[M.Ixed]
instance (Monad m, a ~ a') => SingletonQSM a' (V.Vector a) m q '[False ] where singletonQSM _ _ = simpleM . V.singleton
instance (Monad m, a ~ a') => SingletonQSM a' (V.Vector a) m q '[True  ] where singletonQSM _ _ = resM 0  . singleton

type instance          ModsOf AllocableQSM    (V.Vector a)   = '[M.Ixed]
instance           Monad m => AllocableQSM    (V.Vector a) m q '[False ] where allocQSM _ _ i   = simpleM $ runST $ V.unsafeFreeze =<< MV.unsafeNew i
instance           Monad m => AllocableQSM    (V.Vector a) m q '[True  ] where allocQSM _ _ i   = resM [0..i-1] $ alloc i

type instance          ModsOf ExpandableQSM   (V.Vector a)   = ModsOf GrowableQSM (V.Vector a)
instance          (Monad m, GrowableQM q m (V.Vector a), TransCheck q GrowableInfo ExpandableInfo (V.Vector a))
                           => ExpandableQSM   (V.Vector a) m q s        where expandQSM _ _    = queried (Proxy :: Proxy q) growM' 1

type instance          ModsOf GrowableQSM     (V.Vector a)   = '[M.Ixed]
instance          Monad m  => GrowableQSM     (V.Vector a) m q '[False ] where growQSM _ _ i v  = simpleM $ runST $ V.unsafeThaw v >>= flip MV.unsafeGrow i >>= V.unsafeFreeze
instance          Monad m  => GrowableQSM     (V.Vector a) m q '[True  ] where growQSM _ _ i v  = resM [size v .. size v + i - 1] $ grow i v



-- === Modification ===
-- [+] Appendable
-- [ ] Prependable
-- [ ] Addable
-- [ ] Removable
-- [+] Insertable


type instance            ModsOf AppendableQSM    (V.Vector a)   = '[M.Ixed]
instance   (a ~ a', Monad m) => AppendableQSM a' (V.Vector a) m q '[False ] where appendQSM _ _ el v  = simpleM       $ V.snoc v el
instance   (a ~ a', Monad m) => AppendableQSM a' (V.Vector a) m q '[True  ] where appendQSM _ _ el v  = resM (size v) $ append el v

type instance                       ModsOf InsertableQSM        (V.Vector a)   = '[M.Ixed]
instance   (a ~ a', idx ~ Int, Monad m) => InsertableQSM idx a' (V.Vector a) m q '[False ] where insertQSM _ _ idx el v = simpleM  $ (V.//) v [(idx,el)]
instance   (a ~ a', idx ~ Int, Monad m) => InsertableQSM idx a' (V.Vector a) m q '[True  ] where insertQSM _ _ idx      = resM idx .: insert idx

type instance                       ModsOf FreeableQSM        (V.Vector a) = '[]
instance   (idx ~ Int, Monad m)         => FreeableQSM idx    (V.Vector a) m q s where freeQSM _ _ idx v = insertM' idx (error $ "uninitialized element at index " <> show idx) v


---- === Indexing ===

-- [+] Indexable
-- [ ] TracksElems
-- [ ] TracksIxes
-- [ ] TracksFreeIxes
-- [ ] TracksUsedIxes


type instance                                                 ModsOf IndexableQSM        (V.Vector a)   = '[M.Unchecked, M.Try]
instance   (a ~ a', idx ~ Int, Cond unchecked, Cond try, Monad m) => IndexableQSM idx a' (V.Vector a) m q '[  unchecked,   try] where indexQSM _ _ idx v = simple' <$> checkedBoundsIfM (Proxy :: Proxy unchecked) (Proxy :: Proxy try) idx v (V.unsafeIndex v idx)


--type instance                                                 ModsOf TrackIxesQSM        (V.Vector a)   = '[Unchecked, Try]
--instance   (a ~ a', idx ~ Int, Cond unchecked, Cond try, Monad m) => TrackIxesQSM idx a' (V.Vector a) m q '[unchecked, try] where indexQSM _ _ idx v = simple' <$> checkedBoundsIfM (Proxy :: Proxy unchecked) (Proxy :: Proxy try) idx v (V.unsafeIndex v idx)


type instance            ModsOf TracksElemsQSM    (V.Vector a) = '[]
instance   (a ~ a', Monad m) => TracksElemsQSM a' (V.Vector a) m q s where elemsQSM _ _ = simpleM . V.toList

type instance               ModsOf TracksIxesQSM      (V.Vector a) = '[]
instance   (idx ~ Int, Monad m) => TracksIxesQSM  idx (V.Vector a) m q s where ixesQSM  _ _ v = simpleM $ [0 .. size v - 1]





failT p = ifT p fail error

checkBounds i v l r = if i > max || i < 0 then l ("index " <> show i <> " out of bounds x[0," <> show max <> "]") else r where max = size v - 1
checkBoundsM p idx v = checkBounds idx v (const . failT p) return
checkedBoundsIfM unchecked try idx v = checkedIfM unchecked (checkBoundsM try idx v)

checkedIfM p = ifT p return


checkedIdxIf  cond i f = ifT cond f $ if i < 0 then error "negative index" else f
checkedSizeIf cond i f = ifT cond f $ if i < 0 then error "negative sizeQSM"  else f



---- missing instances ----

instance Default (V.Vector a) where def = mempty










data PA = PA
data PB = PB
data PC = PC

data MA = MA
data MB = MB
data MC = MC

data XR a = XR a

data Opt a = P a
           | N
           deriving (Show)






-- === Knowledge ===

data Knowledge a = Known a 
                 | Unknown
                 deriving (Show)


-- === Info ===

data X_Info idx el cont = X_Info idx el cont

type X_PrimInfo         = 'X_Info 'Unknown     'Unknown
type X_ElInfo        el = 'X_Info 'Unknown     ('Known el)
type X_IdxInfo   idx    = 'X_Info ('Known idx) 'Unknown
type X_IdxElInfo idx el = 'X_Info ('Known idx) ('Known el)

-- === Opts ===

type family X_IdxMod op ix

type family X_ModResult op (info :: X_Info (Knowledge *) (Knowledge *) *) mod

type family GetOpts (m :: [Opt *]) :: [*] where
    GetOpts '[] = '[]
    GetOpts (P a ': ms) = a ': GetOpts ms 
    GetOpts (N   ': ms) = GetOpts ms 



-- === Indexes ===

type family X_IndexOf a


-- === >>> Mods ===

type instance X_ModResult op ('X_Info (Known idx) el cont) M.Ixed = X_IdxMod op idx
type instance X_ModResult op ('X_Info Unknown     el cont) M.Ixed = X_IdxMod op (X_IndexOf cont)


-- -
--type instance X_InfoOf (V.Vector a) = 'X_Info Int a
-- -

type ImpTL = '[Impossible] 

data X_Query  (mods :: [*])     (params :: [*])     = X_Query
data OptQuery (mods :: [Opt *]) (params :: [Opt *]) = OptQuery


-- === Results ===

data X_Res datas a = X_Res datas a deriving (Show, Functor, Traversable, Foldable)

type X_Result op info mods = X_Res (X_Result_ op info mods)

type X_PrimResult  op ms        a = X_Result op (X_PrimInfo         a) ms
type X_ElResult    op ms     el a = X_Result op (X_ElInfo        el a) ms
type X_IdxResult   op ms idx    a = X_Result op (X_IdxInfo   idx    a) ms
type X_IdxElResult op ms idx el a = X_Result op (X_IdxElInfo idx el a) ms

type family X_Result_ op info (mods :: [*]) where
    X_Result_ op info '[]       = ()
    X_Result_ op info (m ': ms) = (X_ModResult op info m, X_Result_ op info ms)



type family X_OptData provided datas opt where
    X_OptData (o ': ps) (d,ds) o = d 
    X_OptData (p ': ps) (d,ds) o = X_OptData ps ds o

type family X_QueryData provided query datas where
    X_QueryData p (q ': qs) d = (X_OptData p d q, X_QueryData p qs d)
    X_QueryData p '[]       d = ()


type X_Result_'  cls info ms t    = RTup2Tup  (t , X_Result_ cls (info (ContainerOf t)) ms)
type X_Result_'' cls info ms t t' = RTup2Tup  (t', X_Result_ cls (info (ContainerOf t)) ms)
type RTup2TupC' cls info ms t = RTup2TupC (t, X_Result_ cls (info (ContainerOf t)) ms)
type RTup2TupC'' cls info ms t t' = RTup2TupC (t', X_Result_ cls (info (ContainerOf t)) ms)


-- utils

type ResultAxioms op info ms t = X_Result_ op (info t) (GetOpts ms) ~ X_Result_ op (info (DataStoreOf t)) (GetOpts ms)


--------------------------------



class X_GetOptData (provided :: [*]) datas opt where getOptData :: Proxy provided -> datas -> Proxy opt -> X_OptData provided datas opt
instance {-# OVERLAPPABLE #-} ( datas ~ (a,as)
                               , X_GetOptData ps as o
                               , X_OptData ps as o ~ X_OptData (p ': ps) (a, as) o
                               )             => X_GetOptData (p ': ps)         datas o   where getOptData _ (a,as) o = getOptData (Proxy :: Proxy ps) as o
instance {-# OVERLAPPABLE #-} datas ~ (a,as) => X_GetOptData (p ': ps)         datas p   where getOptData _ (a,as) _ = a


class    X_GetQueryData (provided :: [*]) (query :: [*]) datas where getQueryData :: Proxy provided -> Proxy query -> datas -> X_QueryData provided query datas
instance {-# OVERLAPPABLE #-} (X_GetQueryData p qs datas, X_GetOptData p datas q)
                           => X_GetQueryData p (q ': qs) datas where getQueryData p q datas = (getOptData p datas (Proxy :: Proxy q), getQueryData p (Proxy :: Proxy qs) datas)
instance {-# OVERLAPPABLE #-} X_GetQueryData p '[]       datas where getQueryData _ _ _     = ()



-- === Mods & Params type families ===

type family XParamsOf op cont :: [*]
type family XModsOf   op cont :: [*]


-----------------------------
-- === Operations classes ===
-----------------------------

-- === Finite ===

-- Measurable
-- MinBounded
-- MaxBounded

class Monad m => X_MeasurableQM ms ps m     cont where x_sizeM     :: X_Query ms ps -> cont -> m (X_PrimResult Y_Measurable ms     (ContainerOf cont) Int)
class Monad m => X_MinBoundedQM ms ps m idx cont where x_minBoundM :: X_Query ms ps -> cont -> m (X_IdxResult  Y_MinBounded ms idx (ContainerOf cont) idx)
class Monad m => X_MaxBoundedQM ms ps m idx cont where x_maxBoundM :: X_Query ms ps -> cont -> m (X_IdxResult  Y_MaxBounded ms idx (ContainerOf cont) idx)

data Y_Measurable        = Y_Measurable
type X_MeasurableM       = X_Simple X_MeasurableQM
type X_MeasurableQ ms ps = X_MeasurableQM ms ps Identity
type X_Measurable        = X_Simple X_MeasurableQM Identity

data Y_MinBounded        = Y_MinBounded
type X_MinBoundedM       = X_Simple X_MinBoundedQM
type X_MinBoundedQ ms ps = X_MinBoundedQM ms ps Identity
type X_MinBounded        = X_Simple X_MinBoundedQM Identity

data Y_MaxBounded        = Y_MaxBounded
type X_MaxBoundedM       = X_Simple X_MaxBoundedQM
type X_MaxBoundedQ ms ps = X_MaxBoundedQM ms ps Identity
type X_MaxBounded        = X_Simple X_MaxBoundedQM Identity

type X_BoundedQM ms ps m idx cont = (X_MinBoundedQM ms ps m idx cont, X_MaxBoundedQM ms ps m idx cont)
type X_BoundedM        m idx cont = X_Simple X_MaxBoundedQM m idx cont
type X_BoundedQ  ms ps m idx cont = X_MaxBoundedQM ms ps Identity idx cont
type X_Bounded           idx cont = X_BoundedM Identity idx cont

-- utils

z_sizeQM     q a = formatResult <$> x_sizeM q a
z_sizeM          = x_queryBuilder z_sizeQM
z_size           = x_withTransFunc (fmap2 runIdentity) z_sizeM

z_minBoundQM q a = formatResult <$> x_minBoundM q a
z_minBoundM      = x_queryBuilder z_minBoundQM
z_minBound       = x_withTransFunc (fmap2 runIdentity) z_minBoundM

z_maxBoundQM q a = formatResult <$> x_maxBoundM q a
z_maxBoundM      = x_queryBuilder z_maxBoundQM
z_maxBound       = x_withTransFunc (fmap2 runIdentity) z_maxBoundM




-- === Construction ===

-- Singleton
-- Allocable
-- Expandable
-- Growable

class Monad m => X_SingletonQM  ms ps m el cont where x_singletonM :: X_Query ms ps         -> el   -> m (X_ElResult   Y_Singleton  ms el (ContainerOf cont) cont)
class Monad m => X_ExpandableQM ms ps m    cont where x_expandM    :: X_Query ms ps         -> cont -> m (X_PrimResult Y_Expandable ms    (ContainerOf cont) cont)
class Monad m => X_AllocableQM  ms ps m    cont where x_allocM     :: X_Query ms ps -> Int          -> m (X_PrimResult Y_Allocable  ms    (ContainerOf cont) cont)
class Monad m => X_GrowableQM   ms ps m    cont where x_growM      :: X_Query ms ps -> Int  -> cont -> m (X_PrimResult Y_Growable   ms    (ContainerOf cont) cont)

data  Y_Singleton         = Y_Singleton
type  X_SingletonM        = X_Simple X_SingletonQM
type  X_SingletonQ  ms ps = X_SingletonQM ms ps Identity
type  X_Singleton         = X_Simple X_SingletonQM Identity

data  Y_Allocable         = Y_Allocable
type  X_AllocableM        = X_Simple X_AllocableQM
type  X_AllocableQ ms ps  = X_AllocableQM ms ps Identity
type  X_Allocable         = X_Simple X_AllocableQM Identity

data  Y_Expandable        = Y_Expandable
type  X_ExpandableM       = X_Simple X_ExpandableQM
type  X_ExpandableQ ms ps = X_ExpandableQM ms ps Identity
type  X_Expandable        = X_Simple X_ExpandableQM Identity

data  Y_Growable          = Y_Growable
type  X_GrowableM         = X_Simple X_GrowableQM
type  X_GrowableQ ms ps   = X_GrowableQM ms ps Identity
type  X_Growable          = X_Simple X_GrowableQM Identity

type instance X_IdxMod Y_Singleton  a = a
type instance X_IdxMod Y_Allocable  a = [a]
type instance X_IdxMod Y_Expandable a = [a]
type instance X_IdxMod Y_Growable   a = [a]

-- utils

z_singletonQM q el  = formatResult <$> x_singletonM q el
z_singletonM        = x_queryBuilder z_singletonQM
z_singleton         = x_withTransFunc (fmap2 runIdentity) z_singletonM

z_allocQM     q i   = formatResult <$> x_allocM q i
z_allocM            = x_queryBuilder z_allocQM
z_alloc             = x_withTransFunc (fmap2 runIdentity) z_allocM

z_expandQM    q a   = formatResult <$> x_expandM q a
z_expandM           = x_queryBuilder z_expandQM
z_expand            = x_withTransFunc (fmap2 runIdentity) z_expandM

z_growQM      q i a = formatResult <$> x_growM q i a
z_growM             = x_queryBuilder z_growQM
z_grow              = x_withTransFunc (fmap3 runIdentity) z_growM



-- === Modification ===
-- Appendable
-- Prependable
-- Addable
-- Removable
-- Insertable
-- Freeable

class Monad m => X_AppendableQM   ms ps m     el cont where x_appendM  :: X_Query ms ps        -> el -> cont -> m (X_ElResult    Y_Appendable    ms     el (ContainerOf cont) cont)
class Monad m => X_PrependableQM  ms ps m     el cont where x_prependM :: X_Query ms ps        -> el -> cont -> m (X_ElResult    Y_Prependable   ms     el (ContainerOf cont) cont)
class Monad m => X_AddableQM      ms ps m     el cont where x_addM     :: X_Query ms ps        -> el -> cont -> m (X_ElResult    Y_Addable       ms     el (ContainerOf cont) cont)
class Monad m => X_RemovableQM    ms ps m     el cont where x_removeM  :: X_Query ms ps        -> el -> cont -> m (X_ElResult    Y_Removable     ms     el (ContainerOf cont) cont)
class Monad m => X_InsertableQM   ms ps m idx el cont where x_insertM  :: X_Query ms ps -> idx -> el -> cont -> m (X_IdxElResult Y_Insertable    ms idx el (ContainerOf cont) cont)
class Monad m => X_FreeableQM     ms ps m idx    cont where x_freeM    :: X_Query ms ps -> idx       -> cont -> m (X_IdxResult   Y_Freeable      ms idx    (ContainerOf cont) cont)

data  Y_Appendable         = Y_Appendable
type  X_AppendableM        = X_Simple X_AppendableQM
type  X_AppendableQ  ms ps = X_AppendableQM ms ps Identity
type  X_Appendable         = X_Simple X_AppendableQM Identity

data  Y_Prependable        = Y_Prependable
type  X_PrependableM       = X_Simple X_PrependableQM
type  X_PrependableQ ms ps = X_PrependableQM ms ps Identity
type  X_Prependable        = X_Simple X_PrependableQM Identity

data  Y_Addable            = Y_Addable
type  X_AddableM           = X_Simple X_AddableQM
type  X_AddableQ     ms ps = X_AddableQM ms ps Identity
type  X_Addable            = X_Simple X_AddableQM Identity

data  Y_Removable          = Y_Removable
type  X_RemovableM         = X_Simple X_RemovableQM
type  X_RemovableQ   ms ps = X_RemovableQM ms ps Identity
type  X_Removable          = X_Simple X_RemovableQM Identity

data  Y_Insertable         = Y_Insertable
type  X_InsertableM        = X_Simple X_InsertableQM
type  X_InsertableQ  ms ps = X_InsertableQM ms ps Identity
type  X_Insertable         = X_Simple X_InsertableQM Identity

data  Y_Freeable           = Y_Freeable
type  X_FreeableM          = X_Simple X_FreeableQM
type  X_FreeableQ    ms ps = X_FreeableQM ms ps Identity
type  X_Freeable           = X_Simple X_FreeableQM Identity

type instance X_IdxMod Y_Appendable  a = a
type instance X_IdxMod Y_Prependable a = a
type instance X_IdxMod Y_Addable     a = a
type instance X_IdxMod Y_Removable   a = a
type instance X_IdxMod Y_Insertable  a = a

z_appendQM  q     el a = formatResult <$> x_appendM q el a
z_appendM              = x_queryBuilder z_appendQM
z_append               = x_withTransFunc (fmap3 runIdentity) z_appendM

z_prependQM q     el a = formatResult <$> x_prependM q el a
z_prependM             = x_queryBuilder z_prependQM
z_prepend              = x_withTransFunc (fmap3 runIdentity) z_prependM

z_addQM     q     el a = formatResult <$> x_addM q el a
z_addM                 = x_queryBuilder z_addQM
z_add                  = x_withTransFunc (fmap3 runIdentity) z_addM

z_removeQM  q     el a = formatResult <$> x_removeM q el a
z_removeM              = x_queryBuilder z_removeQM
z_remove               = x_withTransFunc (fmap3 runIdentity) z_removeM

z_insertQM  q idx el a = formatResult <$> x_insertM q idx el a
z_insertM              = x_queryBuilder z_insertQM
z_insert               = x_withTransFunc (fmap4 runIdentity) z_insertM

z_freeQM    q idx    a = formatResult <$> x_freeM q idx a
z_freeM                = x_queryBuilder z_freeQM
z_free                 = x_withTransFunc (fmap3 runIdentity) z_freeM



---- === Indexing ===

-- Indexable
-- TracksFreeIxes
-- TracksUsedIxes
-- TracksIxes
-- TracksElems

class Monad m => X_IndexableQM      ms ps m idx el cont where x_indexQM    :: X_Query ms ps -> idx -> cont -> m (X_IdxElResult Y_Indexable      ms idx el (ContainerOf cont) el   )
class Monad m => X_TracksFreeIxesQM ms ps m idx    cont where x_freeIxesQM :: X_Query ms ps ->        cont -> m (X_IdxResult   Y_TracksFreeIxes ms idx    (ContainerOf cont) [idx])
class Monad m => X_TracksUsedIxesQM ms ps m idx    cont where x_usedIxesQM :: X_Query ms ps ->        cont -> m (X_IdxResult   Y_TracksUsedIxes ms idx    (ContainerOf cont) [idx])
class Monad m => X_TracksIxesQM     ms ps m idx    cont where x_ixesQM     :: X_Query ms ps ->        cont -> m (X_IdxResult   Y_TracksIxes     ms idx    (ContainerOf cont) [idx])
class Monad m => X_TracksElemsQM    ms ps m     el cont where x_elemsQM    :: X_Query ms ps ->        cont -> m (X_ElResult    Y_TracksElems    ms     el (ContainerOf cont) [el] )

data  Y_Indexable              = Y_Indexable
type  X_IndexableM             = X_Simple X_IndexableQM
type  X_IndexableQ       ms ps = X_IndexableQM ms ps Identity
type  X_Indexable              = X_Simple X_IndexableQM Identity

data  Y_TracksFreeIxes         = Y_TracksFreeIxes
type  X_TracksFreeIxesM        = X_Simple X_TracksFreeIxesQM
type  X_TracksFreeIxesQ  ms ps = X_TracksFreeIxesQM ms ps Identity
type  X_TracksFreeIxes         = X_Simple X_TracksFreeIxesQM Identity

data  Y_TracksUsedIxes         = Y_TracksUsedIxes
type  X_TracksUsedIxesM        = X_Simple X_TracksUsedIxesQM
type  X_TracksUsedIxesQ  ms ps = X_TracksUsedIxesQM ms ps Identity
type  X_TracksUsedIxes         = X_Simple X_TracksUsedIxesQM Identity

data  Y_TracksIxes             = Y_TracksIxes
type  X_TracksIxesM            = X_Simple X_TracksIxesQM
type  X_TracksIxesQ      ms ps = X_TracksIxesQM ms ps Identity
type  X_TracksIxes             = X_Simple X_TracksIxesQM Identity

data  Y_TracksElems            = Y_TracksElems
type  X_TracksElemsM           = X_Simple X_TracksElemsQM
type  X_TracksElemsQ     ms ps = X_TracksElemsQM ms ps Identity
type  X_TracksElems            = X_Simple X_TracksElemsQM Identity

type instance X_IdxMod Y_Indexable   a = a
type instance X_IdxMod Y_TracksElems a = [a]

z_indexQM    q idx a = formatResult <$> x_indexQM q idx a
z_indexM             = x_queryBuilder z_indexQM
z_index              = x_withTransFunc (fmap3 runIdentity) z_indexM

z_freeIxesQM q     a = formatResult <$> x_freeIxesQM q a
z_freeIxesM          = x_queryBuilder z_freeIxesQM
z_freeIxes           = x_withTransFunc (fmap2 runIdentity) z_freeIxesM

z_usedIxesQM q     a = formatResult <$> x_usedIxesQM q a
z_usedIxesM          = x_queryBuilder z_usedIxesQM
z_usedIxes           = x_withTransFunc (fmap2 runIdentity) z_usedIxesM

z_ixesQM     q     a = formatResult <$> x_ixesQM q a
z_ixesM              = x_queryBuilder z_ixesQM
z_ixes               = x_withTransFunc (fmap2 runIdentity) z_ixesM



---------------------
-- === Tunnels === --
---------------------

-- header utils

type OpAxioms op info ms cont = (ResultAxioms op info ms cont, Functor (X_PrimResult op (GetOpts ms) cont))


type family ConstrainCls op (ms :: [Opt *]) (ps :: [Opt *]) (info :: * -> X_Info (Knowledge *) (Knowledge *) *) (m :: * -> *) :: * -> Constraint

#define OpCtx(cls_, info_) {- Inputs (we need to repeat them, cause OSX cpp preprocessor expands the first occurrence only) -} \
                                 cls      ~ (cls_)                                                                             \
                               , info     ~ (info_)                                                                            \
                           {- derives: -}                                                                                      \
                               , fullInfo ~ info cont                                                                          \
                               , matchMs  ~ MatchOpts (XModsOf   cls cont) ms                                                  \
                               , matchPs  ~ MatchOpts (XParamsOf cls cont) ps                                                  \
                               , provided ~ GetOpts matchMs                                                                    \
                               , opts     ~ X_Result_ cls fullInfo provided                                                    \
                               , cont     ~ ContainerOf a                                                                      \
                               , Monad m                                                                                       \
                           {- Super-class constraints: -}                                                                      \
                               , ConstrainCls cls_ matchMs matchPs info m cont                                                 \
                           {- Data queries: -}                                                                                 \
                               , X_QueryData    provided ms opts ~ X_Result_ cls fullInfo ms                                   \
                               , X_GetQueryData provided ms opts                                                               \
                           {- Query-selected result equality: -}                                                               \
                               , ResultAxioms cls info matchMs cont                                                            \



-- === Classes ===


    
runOp (Proxy :: Proxy cls) (Proxy :: Proxy cont) f getter setter (X_Query :: X_Query ms ps) x = do 
    X_Res datas c <- f (OptQuery :: OptQuery (MatchOpts (XModsOf cls cont) ms) (MatchOpts (XParamsOf cls cont) ps)) $ getter x
    return $ X_Res (getQueryData (Proxy :: Proxy (GetOpts (MatchOpts (XModsOf cls cont) ms))) (Proxy :: Proxy ms) datas) $ setter c x

#define RUNOP() runOp (Proxy :: Proxy cls) (Proxy :: Proxy cont)

-- === Finite ===

-- Measurable
-- MinBounded
-- MaxBounded

class Monad m => Y_MeasurableQM ms ps m     cont where xx_sizeM     :: OpAxioms Y_Measurable X_PrimInfo      ms cont => OptQuery ms ps -> cont -> m (X_PrimResult Y_Measurable (GetOpts ms)     cont Int)
class Monad m => Y_MinBoundedQM ms ps m idx cont where xx_minBoundM :: OpAxioms Y_MinBounded (X_IdxInfo idx) ms cont => OptQuery ms ps -> cont -> m (X_IdxResult  Y_MinBounded (GetOpts ms) idx cont idx)
class Monad m => Y_MaxBoundedQM ms ps m idx cont where xx_maxBoundM :: OpAxioms Y_MaxBounded (X_IdxInfo idx) ms cont => OptQuery ms ps -> cont -> m (X_IdxResult  Y_MaxBounded (GetOpts ms) idx cont idx)

type instance ConstrainCls Y_Measurable ms ps info                     m = Y_MeasurableQM ms ps m
type instance ConstrainCls Y_MinBounded ms ps ('X_Info (Known idx) el) m = Y_MinBoundedQM ms ps m idx
type instance ConstrainCls Y_MaxBounded ms ps ('X_Info (Known idx) el) m = Y_MaxBoundedQM ms ps m idx

instance {-# OVERLAPPABLE #-} Monad m                                              => X_MeasurableQM ImpTL ps m     a          where x_sizeM     = impossible
instance {-# OVERLAPPABLE #-} Monad m                                              => X_MeasurableQM ms    ps m     Impossible where x_sizeM     = impossible
instance {-# OVERLAPPABLE #-} (OpCtx(Y_Measurable,X_PrimInfo) , HasContainer a)    => X_MeasurableQM ms    ps m     a          where x_sizeM     = RUNOP() xx_sizeM (view container) const

instance {-# OVERLAPPABLE #-} Monad m                                              => X_MinBoundedQM ImpTL ps m idx a          where x_minBoundM = impossible
instance {-# OVERLAPPABLE #-} Monad m                                              => X_MinBoundedQM ms    ps m idx Impossible where x_minBoundM = impossible
instance {-# OVERLAPPABLE #-} (OpCtx(Y_MinBounded,X_IdxInfo idx) , HasContainer a) => X_MinBoundedQM ms    ps m idx a          where x_minBoundM = RUNOP() xx_minBoundM (view container) const

instance {-# OVERLAPPABLE #-} Monad m                                              => X_MaxBoundedQM ImpTL ps m idx a          where x_maxBoundM = impossible
instance {-# OVERLAPPABLE #-} Monad m                                              => X_MaxBoundedQM ms    ps m idx Impossible where x_maxBoundM = impossible
instance {-# OVERLAPPABLE #-} (OpCtx(Y_MaxBounded,X_IdxInfo idx) , HasContainer a) => X_MaxBoundedQM ms    ps m idx a          where x_maxBoundM = RUNOP() xx_maxBoundM (view container) const
    

-- === Construction ===

-- Singleton
-- Allocable
-- Expandable
-- Growable


class Monad m => Y_SingletonQM  ms ps m el cont where xx_singletonM :: OpAxioms Y_Singleton (X_ElInfo el) ms cont => OptQuery ms ps         -> el   -> m (X_ElResult   Y_Singleton  (GetOpts ms) el cont cont)
class Monad m => Y_AllocableQM  ms ps m    cont where xx_allocM     :: OpAxioms Y_Allocable  X_PrimInfo   ms cont => OptQuery ms ps -> Int          -> m (X_PrimResult Y_Allocable  (GetOpts ms)    cont cont)
class Monad m => Y_ExpandableQM ms ps m    cont where xx_expandM    :: OpAxioms Y_Expandable X_PrimInfo   ms cont => OptQuery ms ps         -> cont -> m (X_PrimResult Y_Expandable (GetOpts ms)    cont cont)
class Monad m => Y_GrowableQM   ms ps m    cont where xx_growM      :: OpAxioms Y_Growable   X_PrimInfo   ms cont => OptQuery ms ps -> Int  -> cont -> m (X_PrimResult Y_Growable   (GetOpts ms)    cont cont)

type instance ConstrainCls Y_Singleton  ms ps ('X_Info idx (Known el)) m = Y_SingletonQM  ms ps m el
type instance ConstrainCls Y_Allocable  ms ps info                     m = Y_AllocableQM  ms ps m
type instance ConstrainCls Y_Expandable ms ps info                     m = Y_ExpandableQM ms ps m
type instance ConstrainCls Y_Growable   ms ps info                     m = Y_GrowableQM   ms ps m

instance {-# OVERLAPPABLE #-} Monad m                                           => X_SingletonQM ImpTL  ps m el a          where x_singletonM = impossible
instance {-# OVERLAPPABLE #-} Monad m                                           => X_SingletonQM ms     ps m el Impossible where x_singletonM = impossible
instance {-# OVERLAPPABLE #-} (OpCtx(Y_Singleton,X_ElInfo el) , IsContainer a)  => X_SingletonQM ms     ps m el a          where x_singletonM = RUNOP() xx_singletonM id (const . fromContainer)

instance {-# OVERLAPPABLE #-} Monad m                                           => X_AllocableQM ImpTL  ps m    a          where x_allocM     = impossible
instance {-# OVERLAPPABLE #-} Monad m                                           => X_AllocableQM ms     ps m    Impossible where x_allocM     = impossible
instance {-# OVERLAPPABLE #-} (OpCtx(Y_Allocable,X_PrimInfo)  , IsContainer a)  => X_AllocableQM ms     ps m    a          where x_allocM     = RUNOP() xx_allocM     id (const . fromContainer)

instance {-# OVERLAPPABLE #-} Monad m                                           => X_ExpandableQM ImpTL ps m    a          where x_expandM    = impossible
instance {-# OVERLAPPABLE #-} Monad m                                           => X_ExpandableQM ms    ps m    Impossible where x_expandM    = impossible
instance {-# OVERLAPPABLE #-} (OpCtx(Y_Expandable,X_PrimInfo) , HasContainer a) => X_ExpandableQM ms    ps m    a          where x_expandM    = RUNOP() xx_expandM (view container) (set container)

instance {-# OVERLAPPABLE #-} Monad m                                           => X_GrowableQM ImpTL ps m      a          where x_growM      = impossible
instance {-# OVERLAPPABLE #-} Monad m                                           => X_GrowableQM ms    ps m      Impossible where x_growM      = impossible
instance {-# OVERLAPPABLE #-} (OpCtx(Y_Growable,X_PrimInfo) , HasContainer a)   => X_GrowableQM ms    ps m      a          where x_growM  q i = RUNOP() (flip xx_growM i) (view container) (set container) q


-- === Modification ===
-- Appendable
-- Prependable
-- Addable
-- Removable
-- Insertable
-- Freeable


class Monad m => Y_AppendableQM  ms ps m     el cont where xx_appendM  :: OpAxioms Y_Appendable  (X_ElInfo        el) ms cont => OptQuery ms ps        -> el -> cont -> m (X_ElResult    Y_Appendable  (GetOpts ms)     el cont cont)
class Monad m => Y_PrependableQM ms ps m     el cont where xx_prependM :: OpAxioms Y_Prependable (X_ElInfo        el) ms cont => OptQuery ms ps        -> el -> cont -> m (X_ElResult    Y_Prependable (GetOpts ms)     el cont cont)
class Monad m => Y_AddableQM     ms ps m     el cont where xx_addM     :: OpAxioms Y_Addable     (X_ElInfo        el) ms cont => OptQuery ms ps        -> el -> cont -> m (X_ElResult    Y_Addable     (GetOpts ms)     el cont cont)
class Monad m => Y_RemovableQM   ms ps m     el cont where xx_removeM  :: OpAxioms Y_Removable   (X_ElInfo        el) ms cont => OptQuery ms ps        -> el -> cont -> m (X_ElResult    Y_Removable   (GetOpts ms)     el cont cont)
class Monad m => Y_InsertableQM  ms ps m idx el cont where xx_insertM  :: OpAxioms Y_Insertable  (X_IdxElInfo idx el) ms cont => OptQuery ms ps -> idx -> el -> cont -> m (X_IdxElResult Y_Insertable  (GetOpts ms) idx el cont cont)
class Monad m => Y_FreeableQM    ms ps m idx    cont where xx_freeM    :: OpAxioms Y_Freeable    (X_IdxInfo   idx   ) ms cont => OptQuery ms ps -> idx       -> cont -> m (X_IdxResult   Y_Freeable    (GetOpts ms) idx    cont cont)

type instance ConstrainCls Y_Appendable  ms ps ('X_Info idx          ('Known el)) m = Y_AppendableQM  ms ps m     el
type instance ConstrainCls Y_Prependable ms ps ('X_Info idx          ('Known el)) m = Y_PrependableQM ms ps m     el
type instance ConstrainCls Y_Addable     ms ps ('X_Info idx          ('Known el)) m = Y_AddableQM     ms ps m     el
type instance ConstrainCls Y_Removable   ms ps ('X_Info idx          ('Known el)) m = Y_RemovableQM   ms ps m     el
type instance ConstrainCls Y_Insertable  ms ps ('X_Info ('Known idx) ('Known el)) m = Y_InsertableQM  ms ps m idx el
type instance ConstrainCls Y_Freeable    ms ps ('X_Info ('Known idx) el         ) m = Y_FreeableQM    ms ps m idx

instance {-# OVERLAPPABLE #-} Monad m                                                  => X_AppendableQM   ImpTL ps m     el a          where x_appendM           = impossible
instance {-# OVERLAPPABLE #-} Monad m                                                  => X_AppendableQM   ms    ps m     el Impossible where x_appendM           = impossible
instance {-# OVERLAPPABLE #-} (HasContainer a, OpCtx(Y_Appendable,X_ElInfo el))        => X_AppendableQM   ms    ps m     el a          where x_appendM  q     el = RUNOP() (flip xx_appendM el) (view container) (set container) q

instance {-# OVERLAPPABLE #-} Monad m                                                  => X_PrependableQM  ImpTL ps m     el a          where x_prependM          = impossible
instance {-# OVERLAPPABLE #-} Monad m                                                  => X_PrependableQM  ms    ps m     el Impossible where x_prependM          = impossible
instance {-# OVERLAPPABLE #-} (HasContainer a, OpCtx(Y_Prependable,X_ElInfo el))       => X_PrependableQM  ms    ps m     el a          where x_prependM q     el = RUNOP() (flip xx_prependM el) (view container) (set container) q

instance {-# OVERLAPPABLE #-} Monad m                                                  => X_AddableQM      ImpTL ps m     el a          where x_addM              = impossible
instance {-# OVERLAPPABLE #-} Monad m                                                  => X_AddableQM      ms    ps m     el Impossible where x_addM              = impossible
instance {-# OVERLAPPABLE #-} (HasContainer a, OpCtx(Y_Addable,X_ElInfo el))           => X_AddableQM      ms    ps m     el a          where x_addM     q     el = RUNOP() (flip xx_addM el) (view container) (set container) q

instance {-# OVERLAPPABLE #-} Monad m                                                  => X_RemovableQM    ImpTL ps m     el a          where x_removeM           = impossible
instance {-# OVERLAPPABLE #-} Monad m                                                  => X_RemovableQM    ms    ps m     el Impossible where x_removeM           = impossible
instance {-# OVERLAPPABLE #-} (HasContainer a, OpCtx(Y_Removable,X_ElInfo el))         => X_RemovableQM    ms    ps m     el a          where x_removeM  q     el = RUNOP() (flip xx_removeM el) (view container) (set container) q

instance {-# OVERLAPPABLE #-} Monad m                                                  => X_InsertableQM   ImpTL ps m idx el a          where x_insertM           = impossible
instance {-# OVERLAPPABLE #-} Monad m                                                  => X_InsertableQM   ms    ps m idx el Impossible where x_insertM           = impossible
instance {-# OVERLAPPABLE #-} (HasContainer a, OpCtx(Y_Insertable,X_IdxElInfo idx el)) => X_InsertableQM   ms    ps m idx el a          where x_insertM  q idx el = RUNOP() (flip (flip xx_insertM idx) el) (view container) (set container) q

instance {-# OVERLAPPABLE #-} Monad m                                                  => X_FreeableQM     ImpTL ps m idx    a          where x_freeM             = impossible
instance {-# OVERLAPPABLE #-} Monad m                                                  => X_FreeableQM     ms    ps m idx    Impossible where x_freeM             = impossible
instance {-# OVERLAPPABLE #-} (HasContainer a, OpCtx(Y_Freeable,X_IdxInfo idx))        => X_FreeableQM     ms    ps m idx    a          where x_freeM    q idx    = RUNOP() (flip xx_freeM idx) (view container) (set container) q




-- === Indexing ===

-- Indexable
-- TracksFreeIxes
-- TracksUsedIxes
-- TracksIxes
-- TracksElems

class Monad m => Y_IndexableQM       ms ps m idx el cont where xx_indexM    :: OpAxioms Y_Indexable      (X_IdxElInfo idx el) ms cont => OptQuery ms ps -> idx -> cont -> m (X_IdxElResult Y_Indexable      (GetOpts ms) idx el cont el   )
class Monad m => Y_TracksFreeIxesQM  ms ps m idx    cont where xx_freeIxesM :: OpAxioms Y_TracksFreeIxes (X_IdxInfo   idx   ) ms cont => OptQuery ms ps        -> cont -> m (X_IdxResult   Y_TracksFreeIxes (GetOpts ms) idx    cont [idx])
class Monad m => Y_TracksUsedIxesQM  ms ps m idx    cont where xx_usedIxesM :: OpAxioms Y_TracksUsedIxes (X_IdxInfo   idx   ) ms cont => OptQuery ms ps        -> cont -> m (X_IdxResult   Y_TracksUsedIxes (GetOpts ms) idx    cont [idx])
class Monad m => Y_TracksIxesQM      ms ps m idx    cont where xx_ixesM     :: OpAxioms Y_TracksIxes     (X_IdxInfo   idx   ) ms cont => OptQuery ms ps        -> cont -> m (X_IdxResult   Y_TracksIxes     (GetOpts ms) idx    cont [idx])
class Monad m => Y_TracksElemsQM     ms ps m     el cont where xx_elemsM    :: OpAxioms Y_TracksElems    (X_ElInfo        el) ms cont => OptQuery ms ps        -> cont -> m (X_ElResult    Y_TracksElems    (GetOpts ms)     el cont [el] )

type instance ConstrainCls Y_Indexable      ms ps ('X_Info ('Known idx) ('Known el)) m = Y_IndexableQM      ms ps m idx el
type instance ConstrainCls Y_TracksFreeIxes ms ps ('X_Info ('Known idx) el         ) m = Y_TracksFreeIxesQM ms ps m idx
type instance ConstrainCls Y_TracksUsedIxes ms ps ('X_Info ('Known idx) el         ) m = Y_TracksUsedIxesQM ms ps m idx
type instance ConstrainCls Y_TracksIxes     ms ps ('X_Info ('Known idx) el         ) m = Y_TracksIxesQM     ms ps m idx
type instance ConstrainCls Y_TracksElems    ms ps ('X_Info idx          ('Known el)) m = Y_TracksElemsQM    ms ps m     el

instance {-# OVERLAPPABLE #-} Monad m                                                  => X_IndexableQM      ImpTL ps m idx el a          where x_indexQM          = impossible
instance {-# OVERLAPPABLE #-} Monad m                                                  => X_IndexableQM      ms    ps m idx el Impossible where x_indexQM          = impossible
instance {-# OVERLAPPABLE #-} (HasContainer a, OpCtx(Y_Indexable,X_IdxElInfo idx el))  => X_IndexableQM      ms    ps m idx el a          where x_indexQM    q idx = RUNOP() (flip xx_indexM idx) (view container) const q

instance {-# OVERLAPPABLE #-} Monad m                                                  => X_TracksFreeIxesQM ImpTL ps m idx    a          where x_freeIxesQM       = impossible
instance {-# OVERLAPPABLE #-} Monad m                                                  => X_TracksFreeIxesQM ms    ps m idx    Impossible where x_freeIxesQM       = impossible
instance {-# OVERLAPPABLE #-} (HasContainer a, OpCtx(Y_TracksFreeIxes,X_IdxInfo idx))  => X_TracksFreeIxesQM ms    ps m idx    a          where x_freeIxesQM       = RUNOP() xx_freeIxesM (view container) const

instance {-# OVERLAPPABLE #-} Monad m                                                  => X_TracksUsedIxesQM ImpTL ps m idx    a          where x_usedIxesQM       = impossible
instance {-# OVERLAPPABLE #-} Monad m                                                  => X_TracksUsedIxesQM ms    ps m idx    Impossible where x_usedIxesQM       = impossible
instance {-# OVERLAPPABLE #-} (HasContainer a, OpCtx(Y_TracksUsedIxes,X_IdxInfo idx))  => X_TracksUsedIxesQM ms    ps m idx    a          where x_usedIxesQM       = RUNOP() xx_usedIxesM (view container) const

instance {-# OVERLAPPABLE #-} Monad m                                                  => X_TracksIxesQM     ImpTL ps m idx    a          where x_ixesQM           = impossible
instance {-# OVERLAPPABLE #-} Monad m                                                  => X_TracksIxesQM     ms    ps m idx    Impossible where x_ixesQM           = impossible
instance {-# OVERLAPPABLE #-} (HasContainer a, OpCtx(Y_TracksIxes,X_IdxInfo idx))      => X_TracksIxesQM     ms    ps m idx    a          where x_ixesQM           = RUNOP() xx_ixesM (view container) const

instance {-# OVERLAPPABLE #-} Monad m                                                  => X_TracksElemsQM    ImpTL ps m     el a          where x_elemsQM          = impossible
instance {-# OVERLAPPABLE #-} Monad m                                                  => X_TracksElemsQM    ms    ps m     el Impossible where x_elemsQM          = impossible
instance {-# OVERLAPPABLE #-} (HasContainer a, OpCtx(Y_TracksElems,X_ElInfo el))       => X_TracksElemsQM    ms    ps m     el a          where x_elemsQM          = RUNOP() xx_elemsM (view container) const






-- ============== --
-- === Vector === --
-- ============== --

type instance X_IndexOf     (V.Vector a) = Int
type instance ContainerOf   (V.Vector a) = V.Vector a
instance      HasContainer  (V.Vector a) where container = id
type instance DataStoreOf   (V.Vector a) = V.Vector a

------------------------
-- === Instances === ---
------------------------

-- === Finite ===

-- [+] Measurable
-- [+] MinBounded
-- [+] MaxBounded

type instance XParamsOf Y_Measurable (V.Vector a) = '[]
type instance XModsOf   Y_Measurable (V.Vector a) = '[]

type instance XParamsOf Y_MinBounded (V.Vector a) = '[]
type instance XModsOf   Y_MinBounded (V.Vector a) = '[]

type instance XParamsOf Y_MaxBounded (V.Vector a) = '[]
type instance XModsOf   Y_MaxBounded (V.Vector a) = '[]

instance Monad m              => Y_MeasurableQM '[] ps m     (V.Vector a) where xx_sizeM     _   = return . X_Res () . V.length
instance (Monad m, idx ~ Int) => Y_MinBoundedQM '[] ps m idx (V.Vector a) where xx_minBoundM _ _ = return $ X_Res () 0
instance (Monad m, idx ~ Int) => Y_MaxBoundedQM '[] ps m idx (V.Vector a) where xx_maxBoundM _ v = return $ X_Res () $ V.length v - 1


-- === Construction ===

-- [+] Singleton
-- [+] Allocable
-- [+] Expandable
-- [+] Growable

type instance XParamsOf Y_Singleton  (V.Vector a) = '[]
type instance XModsOf   Y_Singleton  (V.Vector a) = '[M.Ixed]

type instance XParamsOf Y_Allocable  (V.Vector a) = '[]
type instance XModsOf   Y_Allocable  (V.Vector a) = '[M.Ixed]

type instance XParamsOf Y_Expandable (V.Vector a) = '[]
type instance XModsOf   Y_Expandable (V.Vector a) = '[M.Ixed]

type instance XParamsOf Y_Growable   (V.Vector a) = '[]
type instance XModsOf   Y_Growable   (V.Vector a) = '[M.Ixed]

instance (Monad m, a ~ a') => Y_SingletonQM '[N       ] ps m a' (V.Vector a) where xx_singletonM _ = return . X_Res ()     . V.singleton
instance (Monad m, a ~ a') => Y_SingletonQM '[P M.Ixed] ps m a' (V.Vector a) where xx_singletonM _ = return . X_Res (0,()) . V.singleton

instance Monad m =>           Y_AllocableQM '[N       ] ps m    (V.Vector a) where xx_allocM _ i = return $ X_Res ()            $ runST $ V.unsafeFreeze =<< MV.unsafeNew i
instance Monad m =>           Y_AllocableQM '[P M.Ixed] ps m    (V.Vector a) where xx_allocM _ i = return $ X_Res ([0..i-1],()) $ runST $ V.unsafeFreeze =<< MV.unsafeNew i

instance Monad m =>           Y_ExpandableQM '[N       ] ps m   (V.Vector a) where xx_expandM _ v = return $ X_Res ()                $ runST $ V.unsafeThaw v >>= flip MV.unsafeGrow 1 >>= V.unsafeFreeze
instance Monad m =>           Y_ExpandableQM '[P M.Ixed] ps m   (V.Vector a) where xx_expandM _ v = return $ X_Res ([V.length v],()) $ runST $ V.unsafeThaw v >>= flip MV.unsafeGrow 1 >>= V.unsafeFreeze

instance Monad m =>           Y_GrowableQM   '[N       ] ps m   (V.Vector a) where xx_growM _ i v = return $ X_Res ()                                      $ runST $ V.unsafeThaw v >>= flip MV.unsafeGrow i >>= V.unsafeFreeze
instance Monad m =>           Y_GrowableQM   '[P M.Ixed] ps m   (V.Vector a) where xx_growM _ i v = return $ X_Res ([V.length v .. V.length v + i - 1],()) $ runST $ V.unsafeThaw v >>= flip MV.unsafeGrow i >>= V.unsafeFreeze


-- === Modification ===
-- [+] Appendable
-- [+] Prependable
-- [+] Addable
-- [+] Removable
-- [+] Insertable
-- [+] Freeable


type instance XParamsOf Y_Appendable  (V.Vector a) = '[]
type instance XModsOf   Y_Appendable  (V.Vector a) = '[M.Ixed]

type instance XParamsOf Y_Prependable (V.Vector a) = '[]
type instance XModsOf   Y_Prependable (V.Vector a) = '[M.Ixed]

type instance XParamsOf Y_Addable     (V.Vector a) = '[]
type instance XModsOf   Y_Addable     (V.Vector a) = '[M.Ixed]

type instance XParamsOf Y_Removable   (V.Vector a) = '[M.Try]
type instance XModsOf   Y_Removable   (V.Vector a) = '[M.Ixed]

type instance XParamsOf Y_Insertable  (V.Vector a) = '[]
type instance XModsOf   Y_Insertable  (V.Vector a) = '[M.Ixed]

type instance XParamsOf Y_Freeable    (V.Vector a) = '[]
type instance XModsOf   Y_Freeable    (V.Vector a) = '[]

instance (Monad m, a ~ a')       => Y_AppendableQM  '[N       ] ps m a' (V.Vector a) where xx_appendM  _ el v = (return . X_Res ())            $ V.snoc v el
instance (Monad m, a ~ a')       => Y_AppendableQM  '[P M.Ixed] ps m a' (V.Vector a) where xx_appendM  _ el v = (return . X_Res (z_size v,())) $ V.snoc v el

instance (Monad m, a ~ a')       => Y_PrependableQM '[N       ] ps m a' (V.Vector a) where xx_prependM _ el v = (return . X_Res ())     $ V.cons el v
instance (Monad m, a ~ a')       => Y_PrependableQM '[P M.Ixed] ps m a' (V.Vector a) where xx_prependM _ el v = (return . X_Res (0,())) $ V.cons el v

instance (Monad m, a ~ a')       => Y_AddableQM     '[N       ] ps m a' (V.Vector a) where xx_addM     _ el v = (return . X_Res ())            $ V.snoc v el
instance (Monad m, a ~ a')       => Y_AddableQM     '[P M.Ixed] ps m a' (V.Vector a) where xx_addM     _ el v = (return . X_Res (z_size v,())) $ V.snoc v el

instance (Monad m, Eq a, a ~ a') => Y_RemovableQM   '[N       ] '[P M.Try] m a' (V.Vector a) where xx_removeM _ el v = case idx of
                                                                                                                           Just  i -> (return . X_Res ()) $ V.slice 0 (i-1) v <> V.slice i (z_size v - i) v
                                                                                                                           Nothing -> fail "Element not found"
                                                                                                                       where idx = V.findIndex (== el) v
instance (Monad m, Eq a, a ~ a') => Y_RemovableQM '[P M.Ixed] '[P M.Try] m a' (V.Vector a) where xx_removeM _ el v = case idx of
                                                                                                                         Just  i -> (return . X_Res (i,())) $ V.slice 0 (i-1) v <> V.slice i (z_size v - i) v
                                                                                                                         Nothing -> fail "Element not found"
                                                                                                                     where idx = V.findIndex (== el) v


instance (Monad m, a ~ a', idx ~ Int) => Y_InsertableQM '[N       ] ps m idx a' (V.Vector a) where xx_insertM _ idx el v = (return . X_Res ())       $ (V.//) v [(idx,el)]
instance (Monad m, a ~ a', idx ~ Int) => Y_InsertableQM '[P M.Ixed] ps m idx a' (V.Vector a) where xx_insertM _ idx el v = (return . X_Res (idx,())) $ (V.//) v [(idx,el)]

instance (Monad m, idx ~ Int) => Y_FreeableQM '[] ps m idx (V.Vector a) where xx_freeM _ idx v = (return . X_Res ())       $ (V.//) v [(idx,error $ "uninitialized element at index " <> show idx)]



-- === Indexing ===

-- [+] Indexable
-- [-] TracksFreeIxes
-- [-] TracksUsedIxes
-- [+] TracksIxes
-- [+] TracksElems

type instance XParamsOf Y_Indexable       (V.Vector a) = '[M.Unchecked, M.Try]
type instance XModsOf   Y_Indexable       (V.Vector a) = '[M.Ixed]

type instance XParamsOf Y_TracksFreeIxes  (V.Vector a) = '[]
type instance XModsOf   Y_TracksFreeIxes  (V.Vector a) = '[]

instance (Monad m, a ~ a', idx ~ Int, Cond2 unchecked, Cond2 try) => Y_IndexableQM  '[N       ] '[unchecked, try] m idx a' (V.Vector a) where xx_indexM  _ idx v = X_Res ()       <$> checkedBoundsIfM2 (Proxy :: Proxy unchecked) (Proxy :: Proxy try) idx v (V.unsafeIndex v idx)
instance (Monad m, a ~ a', idx ~ Int, Cond2 unchecked, Cond2 try) => Y_IndexableQM  '[P M.Ixed] '[unchecked, try] m idx a' (V.Vector a) where xx_indexM  _ idx v = X_Res (idx,()) <$> checkedBoundsIfM2 (Proxy :: Proxy unchecked) (Proxy :: Proxy try) idx v (V.unsafeIndex v idx)

instance (Monad m, idx ~ Int)                                     => Y_TracksIxesQM  '[]        ps                m idx    (V.Vector a) where xx_ixesM   _     v = (return . X_Res ()) $ [0 .. z_size v -1]
instance (Monad m, a ~ a')                                        => Y_TracksElemsQM '[]        ps                m     a' (V.Vector a) where xx_elemsM  _     v = (return . X_Res ()) $ V.toList v





failT2 p = ifT2 p fail error

checkBounds2 i v l r = if i > max || i < 0 then l ("index " <> show i <> " out of bounds x[0," <> show max <> "]") else r where max = size v - 1
checkBoundsM2 p idx v = checkBounds2 idx v (const . failT2 p) return
checkedBoundsIfM2 unchecked try idx v = checkedIfM2 unchecked (checkBoundsM2 try idx v)

checkedIfM2 p = ifT2 p return


class    Cond2 (opt :: Opt *) where ifT2 :: Proxy opt -> a -> a -> a
instance Cond2 (P a)          where ifT2 _ = const
instance Cond2 N              where ifT2 _ = flip const







-- ================ --
-- === Reusable === --
-- ================ --

data X_Reusable idx a = X_Reusable [idx] !a deriving (Show, Functor, Foldable, Traversable)

type instance X_IndexOf    (X_Reusable idx a) = X_IndexOf (ContainerOf a)
type instance ContainerOf  (X_Reusable idx a) = X_Reusable idx a
type instance DataStoreOf  (X_Reusable idx a) = ContainerOf a

instance      HasContainer (X_Reusable idx a) where container     = id
instance      IsContainer  (X_Reusable idx a) where fromContainer = id

type instance Unlayered (X_Reusable idx a) = a
instance      Layered   (X_Reusable idx a) where layered = lens (\(X_Reusable _ a) -> a) (\(X_Reusable ixs _) a -> X_Reusable ixs a)

instance      (IsContainer a, FromList (ContainerOf a)) 
           => FromList  (X_Reusable idx a) where fromList = X_Reusable mempty . fromContainer . fromList
type instance Item      (X_Reusable idx a) = Item (ContainerOf a)

------------------------
-- === Instances === ---
------------------------

-- === Finite ===

-- [+] Measurable
-- [+] MinBounded
-- [+] MaxBounded

type instance XParamsOf Y_Measurable (X_Reusable idx a) = XParamsOf Y_Measurable (ContainerOf a)
type instance XModsOf   Y_Measurable (X_Reusable idx a) = XModsOf   Y_Measurable (ContainerOf a)

type instance XParamsOf Y_MinBounded (X_Reusable idx a) = XParamsOf Y_MinBounded (ContainerOf a)
type instance XModsOf   Y_MinBounded (X_Reusable idx a) = XModsOf   Y_MinBounded (ContainerOf a)

type instance XParamsOf Y_MaxBounded (X_Reusable idx a) = XParamsOf Y_MaxBounded (ContainerOf a)
type instance XModsOf   Y_MaxBounded (X_Reusable idx a) = XModsOf   Y_MaxBounded (ContainerOf a)

instance (X_MeasurableQM (GetOpts ms) (GetOpts ps) m     a)             => Y_MeasurableQM ms ps m     (X_Reusable idx  a) where xx_sizeM     _ = x_sizeM     (X_Query :: X_Query (GetOpts ms) (GetOpts ps)) . unlayer
instance (X_MinBoundedQM (GetOpts ms) (GetOpts ps) m idx a, idx ~ idx') => Y_MinBoundedQM ms ps m idx (X_Reusable idx' a) where xx_minBoundM _ = x_minBoundM (X_Query :: X_Query (GetOpts ms) (GetOpts ps)) . unlayer
instance (X_MaxBoundedQM (GetOpts ms) (GetOpts ps) m idx a, idx ~ idx') => Y_MaxBoundedQM ms ps m idx (X_Reusable idx' a) where xx_maxBoundM _ = x_maxBoundM (X_Query :: X_Query (GetOpts ms) (GetOpts ps)) . unlayer


-- === Construction ===

-- [+] Singleton
-- [+] Allocable
-- [+] Expandable
-- [+] Growable

type instance XParamsOf Y_Singleton  (X_Reusable idx a) = XParamsOf Y_Singleton  (ContainerOf a)
type instance XModsOf   Y_Singleton  (X_Reusable idx a) = XModsOf   Y_Singleton  (ContainerOf a)

type instance XParamsOf Y_Allocable  (X_Reusable idx a) = XParamsOf Y_Allocable  (ContainerOf a)
type instance XModsOf   Y_Allocable  (X_Reusable idx a) = XModsOf   Y_Allocable  (ContainerOf a)

type instance XParamsOf Y_Expandable (X_Reusable idx a) = XParamsOf Y_Expandable (ContainerOf a)
type instance XModsOf   Y_Expandable (X_Reusable idx a) = XModsOf   Y_Expandable (ContainerOf a)

type instance XParamsOf Y_Growable   (X_Reusable idx a) = XParamsOf Y_Growable   (ContainerOf a)
type instance XModsOf   Y_Growable   (X_Reusable idx a) = XModsOf   Y_Growable   (ContainerOf a)

instance ( X_SingletonQM (M.Ixed ': GetOpts ms) (GetOpts ps) m el a, idx ~ X_IndexOf (ContainerOf a)) => Y_SingletonQM ms ps m el (X_Reusable idx a) where 
    xx_singletonM _ el = do X_Res (ix,ds) r <- x_singletonM (X_Query :: X_Query (M.Ixed ': GetOpts ms) (GetOpts ps)) el
                            return $ X_Res ds $ X_Reusable [ix] r

instance ( X_AllocableQM (M.Ixed ': GetOpts ms) (GetOpts ps) m a, idx ~ X_IndexOf (ContainerOf a)) => Y_AllocableQM ms ps m (X_Reusable idx a) where 
    xx_allocM _ i = do X_Res (ixs,ds) r <- x_allocM (X_Query :: X_Query (M.Ixed ': GetOpts ms) (GetOpts ps)) i
                       return $ X_Res ds $ X_Reusable ixs r

instance ( X_ExpandableQM (M.Ixed ': GetOpts ms) (GetOpts ps) m a, idx ~ X_IndexOf (ContainerOf a)) => Y_ExpandableQM ms ps m (X_Reusable idx a) where 
    xx_expandM _ (X_Reusable ixs a) = do X_Res (ixs',ds) r <- x_expandM (X_Query :: X_Query (M.Ixed ': GetOpts ms) (GetOpts ps)) a
                                         return $ X_Res ds $ X_Reusable (ixs <> ixs') r

instance ( X_GrowableQM (M.Ixed ': GetOpts ms) (GetOpts ps) m a, idx ~ X_IndexOf (ContainerOf a)) => Y_GrowableQM ms ps m (X_Reusable idx a) where 
    xx_growM _ i (X_Reusable ixs a) = do X_Res (ixs',ds) r <- x_growM (X_Query :: X_Query (M.Ixed ': GetOpts ms) (GetOpts ps)) i a
                                         return $ X_Res ds $ X_Reusable (ixs <> ixs') r








-- === OptBuilder ===

x_optBuilder :: f -> X_OptBuilderBase f
x_optBuilder = X_OptBuilder


newtype X_OptBuilder (mods :: [*]) (params :: [*]) a = X_OptBuilder a deriving (Show, Functor)
type    X_OptBuilderBase = X_OptBuilder '[] '[]

class                                                X_FuncTrans mods params f a | a mods params -> f          where x_transFunc :: X_OptBuilder mods params f -> a
instance (mods ~ mods', params ~ params', f ~ f') => X_FuncTrans mods params f (X_OptBuilder mods' params' f') where x_transFunc = id
instance (f ~ (X_Query mods params -> a -> b))    => X_FuncTrans mods params f (a -> b)                        where x_transFunc (X_OptBuilder f) = f X_Query

class                                                                     X_FuncBuilder f     a | a -> f                      where x_buildFunc :: f -> a
instance {-# OVERLAPPABLE #-} (f ~ a, g ~ b)                           => X_FuncBuilder (f -> g) (a -> b)                     where x_buildFunc = id
instance {-# OVERLAPPABLE #-} (t ~ (f -> g), mods ~ '[], params ~ '[]) => X_FuncBuilder (f -> g) (X_OptBuilder mods params t) where x_buildFunc = X_OptBuilder



x_queryBuilder = x_transFunc . x_optBuilder

x_extendOptBuilder :: X_Query newMods newParams -> X_Query collectedMods collectedParams -> X_OptBuilder mods params a -> X_OptBuilder (Concat newMods   (Concat collectedMods   mods  ))
                                                                                                                                       (Concat newParams (Concat collectedParams params))
                                                                                                                                       a
x_extendOptBuilder _ _ (X_OptBuilder a) = X_OptBuilder a


x_ixed      = x_queryBuilder $ x_transFunc .: x_extendOptBuilder (X_Query :: X_Query '[ M.Ixed ] '[])

x_raw       = x_queryBuilder $ x_transFunc .: x_extendOptBuilder (X_Query :: X_Query '[] '[ M.Raw       ])

x_try       = x_queryBuilder $ x_transFunc .: x_extendOptBuilder (X_Query :: X_Query '[] '[ M.Try       ])
x_unchecked = x_queryBuilder $ x_transFunc .: x_extendOptBuilder (X_Query :: X_Query '[] '[ M.Unchecked ])
x_unsafe    = x_queryBuilder $ x_transFunc .: x_extendOptBuilder (X_Query :: X_Query '[] '[ M.Unsafe    ])





x_withTransFunc = x_transFunc .: x_appFunc

x_appFunc :: (f -> g) -> X_OptBuilder ms ps f -> X_OptBuilder ms ps g
x_appFunc = fmap





-- ===

type family MakeOpt (ps :: [*]) :: [Opt *] where
    MakeOpt '[]       = '[]
    MakeOpt (p ': ps) = 'P p ': MakeOpt ps

type family MatchOpts (provided :: [*]) (selected :: [*]) :: [Opt *] where
    MatchOpts (p ': ps) sel = (p `X_In` sel) ': MatchOpts ps sel 
    MatchOpts '[]       sel = '[]


type family X_In flag flags :: Opt * where
    X_In f (f  ': fs) = P f 
    X_In f (f' ': fs) = X_In f fs
    X_In f '[]        = N  







-------------------------------------------------

data L1 a = L1 a deriving (Show)
type instance (ContainerOf (L1 a)) = a
instance HasContainer (L1 a) where
    container = lens (\(L1 a) -> a) (\(L1 _) a -> L1 a)
instance IsContainer (L1 a)
instance Wrapped (L1 a) where
    type Unwrapped (L1 a) = a
    _Wrapped' = iso (\(L1 a) -> a) L1

type X_Func  ms ps f = (X_FuncTrans '[] '[] (X_Query ms ps -> f) a => a)









main = do
    (v1,i,_) <- z_singletonQM (X_Query :: X_Query '[M.Ixed,M.Ixed] '[]) 5 :: IO (L1 (V.Vector Int), Int, Int)
    print (v1,i)

    print " ---------------------- "

    let v2 = fromList [1,2,3] :: X_Reusable Int (L1 (V.Vector Int))

    putStrLn $ "v2: "           <> show v2
    putStrLn $ "size: "         <> show (z_size     v2)
    putStrLn $ "min bounds: "   <> show (z_minBound v2)
    putStrLn $ "max bounds: "   <> show (z_maxBound v2)
    --putStrLn $ "after expand: " <> show (z_expand   v2)
    --putStrLn $ "after grow: "   <> show (z_grow 10  v2)
 
    --s <- z_sizeM v2 
    --print s

    --print =<< x_ixed x_ixed z_expandM v2
    
    print "END"



resToRTup (X_Res ds a) = (a,ds)


--xxx :: X_ExpandableM m t => t -> m t
xxx :: X_Ixed X_ExpandableM m t => t -> m (t, [X_IndexOf (ContainerOf t)])
xxx v = x_ixed z_expandM v


type family RTup2Tup rt
type instance RTup2Tup () = ()
type instance RTup2Tup (t1,()) = t1
type instance RTup2Tup (t1,(t2,())) = (t1, t2)
type instance RTup2Tup (t1,(t2,(t3,()))) = (t1, t2, t3)
type instance RTup2Tup (t1,(t2,(t3,(t4,())))) = (t1, t2, t3, t4)

type family Tup2RTup t where
    Tup2RTup ()               = () 
    Tup2RTup (t1, t2)         = (t1,(t2,())) 
    Tup2RTup (t1, t2, t3)     = (t1,(t2,(t3,()))) 
    Tup2RTup (t1, t2, t3, t4) = (t1,(t2,(t3,(t4,())))) 
    Tup2RTup a                = (a,())



class Tup2RTup (RTup2Tup rt) ~ rt => RTup2TupC rt where rtup2tup :: rt -> RTup2Tup rt 
instance RTup2TupC () where rtup2tup = id 
instance Tup2RTup t1 ~ (t1,()) => RTup2TupC (t1,()) where rtup2tup (t1,()) = t1 
instance RTup2TupC (t1,(t2,())) where rtup2tup (t1,(t2,())) = (t1,t2) 
instance RTup2TupC (t1,(t2,(t3,()))) where rtup2tup (t1,(t2,(t3,()))) = (t1,t2,t3) 
instance RTup2TupC (t1,(t2,(t3,(t4,())))) where rtup2tup (t1,(t2,(t3,(t4,())))) = (t1,t2,t3,t4) 


class MatchRTT rt t | rt -> t, t -> rt
instance (rt ~ Tup2RTup t, t ~ RTup2Tup rt) => MatchRTT rt t

--

formatResult = rtup2tupX . resToRTup

class rt ~ Tup2RTup t => RTup2TupX rt t | rt -> t where rtup2tupX :: rt -> t
instance {-# OVERLAPPABLE #-}                          RTup2TupX () () where rtup2tupX = id
instance {-# OVERLAPPABLE #-} Tup2RTup t1 ~ (t1,()) => RTup2TupX (t1,()) t1 where rtup2tupX (t1,()) = t1
instance {-# OVERLAPPABLE #-}                          RTup2TupX (t1,(t2,())) (t1,t2) where rtup2tupX (t1,(t2,())) = (t1,t2)
instance {-# OVERLAPPABLE #-}                          RTup2TupX (t1,(t2,(t3,()))) (t1,t2,t3) where rtup2tupX (t1,(t2,(t3,()))) = (t1,t2,t3)




--class (rt ~ Tup2RTup t, t ~ RTup2Tup rt) => TupConv rt t where
--    rttconv :: Iso' rt t

--instance                         TupConv ()     () where rttconv = iso id id
--instance Tup2RTup t ~ (t, ()) => TupConv (t,()) t  where rttconv = undefined


--foo = rttconv 1

--class rt ~ Tup2RTup t => RTup2TupX rt t | rt -> t where
--    rtup2tupX :: rt -> t

--foo :: _ => _
--foo = rtup2tupX 1


--instance (rt ~ Tup2RTup t, t ~ RTup2Tup rt) => MatchRTT rt t

-------------------------------------------------

--instance (Monad m, ExpandableQM (Mods.Ixed ': q) m a, idx ~ IndexOf' (DataStoreOf a)) => ExpandableQSM (HReusable idx a) m q s where
--    expandQSM _ _ c = do
--        (ixs, r) <- splitResData <$> nested layered ((ixed . queried (Proxy :: Proxy q)) expandM') c
--        return $ fmap (withIxes' (<> ixs)) r




--instance (
--        X_Result' ('X_Info Int a) ms ~ X_Result' ('X_Info Int a) (GetOpts (MakeOpt ms))
--       , XV_Expandable (MakeOpt ms) (MakeOpt ps) m
--       ) => X_Expandable ms ps a m (V.Vector a) where 
--  x_expandM _ _ = xv_expandM (Proxy :: Proxy (MakeOpt ms)) (Proxy :: Proxy (MakeOpt ps))

--class XV_Expandable (mods :: [Opt *]) (parms :: [Opt *]) m where
--    xv_expandM :: cont ~ V.Vector a => Proxy mods -> Proxy parms -> cont -> m (X_Result (X_InfoOf cont) (GetOpts mods) cont)

--instance Monad m => XV_Expandable '[N       ] ps m where xv_expandM _ _ v = return $ X_Res ()              $ runST $ V.unsafeThaw v >>= flip MV.unsafeGrow 1 >>= V.unsafeFreeze
--instance Monad m => XV_Expandable '[P M.Ixed] ps m where xv_expandM _ _ v = return $ X_Res (V.length v,()) $ runST $ V.unsafeThaw v >>= flip MV.unsafeGrow 1 >>= V.unsafeFreeze



--instance 

--tst v = x_expandM (Proxy :: Proxy '[]) (Proxy :: Proxy '[]) v 
--simpleM $ runST $ V.unsafeThaw v >>= flip MV.unsafeGrow i >>= V.unsafeFreeze

--class ExpandableQSM            cont m q s where expandQSM    :: (AssumeQuery info q s, info ~ ExpandableInfo    cont) => Query q s -> info ->        cont -> SelResultM info s m cont


--instance {-# OVERLAPPABLE #-} X_Appendable ms ps                           el m cont
--instance {-# OVERLAPPABLE #-} X_Appendable ms (P PA ': ps)                 el m cont
--instance {-# OVERLAPPABLE #-} X_Appendable ms (P PA ': P PB ': ps)         el m cont
--instance {-# OVERLAPPABLE #-} X_Appendable ms (P PA ': P PB ': P PC ': ps) el m cont
--instance {-# OVERLAPPABLE #-} X_Appendable ms (P PA ': N    ': P PC ': ps) el m cont


--instance {-# OVERLAPPABLE #-} X_Appendable ms ps 


--type instance            ModsOf AppendableQSM    (V.Vector a)   = '[M.Ixed]
--instance   (a ~ a', Monad m) => AppendableQSM a' (V.Vector a) m q '[False ] where appendQSM _ _ el v  = simpleM       $ V.snoc v el
--instance   (a ~ a', Monad m) => AppendableQSM a' (V.Vector a) m q '[True  ] where appendQSM _ _ el v  = resM (size v) $ append el v