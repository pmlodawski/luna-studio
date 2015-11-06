{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE FunctionalDependencies    #-}


module Data.Container.Class where

import Prologue hiding (Indexable, index, Bounded, Ixed, Simple, Indexed)

import qualified Data.Container.Opts as Opts
import           Data.Container.Opts (Query(..), Opt(..), Knowledge(..), queryBuilder, withTransFunc)
import           Data.Container.Poly (Simple)


data Impossible    = Impossible  deriving (Show)
data ImpossibleM a = ImpossibleM deriving (Show, Functor)
type ImpTL = '[Impossible] 



impossible = error "Impossible happened."

----------------------------
-- === General utilities ===
----------------------------

type family ContainerOf a
type family DataStoreOf a
type family IndexOf     a

type  HasContainer = HasContainerM Identity
class HasContainerM m a where
    viewContainerM :: a -> m (ContainerOf a)
    setContainerM  :: ContainerOf a -> a -> m a
    default viewContainerM :: (ContainerOf a ~ ContainerOf (Unwrapped a), Wrapped a, HasContainerM m (Unwrapped a)) => a -> m (ContainerOf a)
    viewContainerM = viewContainerM . unwrap'
    default setContainerM :: (ContainerOf a ~ ContainerOf (Unwrapped a), Wrapped a, HasContainerM m (Unwrapped a), Functor m) => ContainerOf a -> a -> m a
    setContainerM = wrapped' . setContainerM

type                    IsContainer  = IsContainerM Identity
class HasContainerM m a => IsContainerM m a where
    fromContainerM :: ContainerOf a -> m a
    default fromContainerM :: (Unwrapped a ~ ContainerOf a, Wrapped a, Monad m) => ContainerOf a -> m a
    fromContainerM = return . view unwrapped'


container :: HasContainer a => Lens' a (ContainerOf a)
container = lens (runIdentity . viewContainerM) (runIdentity .: flip setContainerM)

fromContainer :: IsContainer a => ContainerOf a -> a
fromContainer = runIdentity . fromContainerM

withContainerM :: (Monad m, HasContainerM m a) => (ContainerOf a -> m (ContainerOf a)) -> a -> m a
withContainerM f l = viewContainerM l >>= f >>= flip setContainerM l

withContainerM' :: (Monad m, HasContainerM m a) => (ContainerOf a -> ContainerOf a) -> a -> m a
withContainerM' = withContainerM . (return .)

-- Type level utilities

type family PrimStoreOf    a where PrimStoreOf    a = PrimStoreOf' (DataStoreOf a) a
type family PrimStoreOf' t a where PrimStoreOf' a a = a
                                   PrimStoreOf' t a = PrimStoreOf' (DataStoreOf (ContainerOf (DataStoreOf a))) (ContainerOf (DataStoreOf a))


----------------------------
-- === Results & Infos ===
----------------------------

-- === Info ===

data Info idx el cont = Info idx el cont

type PrimInfo         = 'Info 'Unknown     'Unknown
type ElInfo        el = 'Info 'Unknown     ('Known el)
type IdxInfo   idx    = 'Info ('Known idx) 'Unknown
type IdxElInfo idx el = 'Info ('Known idx) ('Known el)



-- === Results ===

data Res datas a = Res datas a deriving (Show, Functor, Traversable, Foldable)

type Result op info mods = Res (Result_ op info mods)

type PrimResult  op ms        a = Result op (PrimInfo         a) ms
type ElResult    op ms     el a = Result op (ElInfo        el a) ms
type IdxResult   op ms idx    a = Result op (IdxInfo   idx    a) ms
type IdxElResult op ms idx el a = Result op (IdxElInfo idx el a) ms

type family Result_ op info (mods :: [*]) where
    Result_ op info '[]       = ()
    Result_ op info (m ': ms) = (ModResult op info m, Result_ op info ms)



-- === Opts ===

type family IdxMod op ix

type family ModResult op (info :: Info (Knowledge *) (Knowledge *) *) mod

type family GetOpts (m :: [Opt *]) :: [*] where
    GetOpts '[] = '[]
    GetOpts (P a ': ms) = a ': GetOpts ms 
    GetOpts (N   ': ms) = GetOpts ms 



-- === >>> Mods ===

type instance ModResult op ('Info (Known idx) el cont) Opts.Ixed = IdxMod op idx
type instance ModResult op ('Info Unknown     el cont) Opts.Ixed = IdxMod op (IndexOf cont)


-----------------------------
-- === Operations classes ===
-----------------------------

-- === Finite ===

-- Measurable
-- MinBounded
-- MaxBounded

class Ctx ms m cont => MeasurableQM ms ps m     cont where sizeQM     :: Query ms ps -> cont -> m (PrimResult MeasurableOp ms     (ContainerOf cont) Int)
class Ctx ms m cont => MinBoundedQM ms ps m idx cont where minBoundQM :: Query ms ps -> cont -> m (IdxResult  MinBoundedOp ms idx (ContainerOf cont) idx)
class Ctx ms m cont => MaxBoundedQM ms ps m idx cont where maxBoundQM :: Query ms ps -> cont -> m (IdxResult  MaxBoundedOp ms idx (ContainerOf cont) idx)

data MeasurableOp      = MeasurableOp
type MeasurableM       = Simple MeasurableQM
type MeasurableQ ms ps = MeasurableQM ms ps Identity
type Measurable        = Simple MeasurableQM Identity

data MinBoundedOp      = MinBoundedOp
type MinBoundedM       = Simple MinBoundedQM
type MinBoundedQ ms ps = MinBoundedQM ms ps Identity
type MinBounded        = Simple MinBoundedQM Identity

data MaxBoundedOp      = MaxBoundedOp
type MaxBoundedM       = Simple MaxBoundedQM
type MaxBoundedQ ms ps = MaxBoundedQM ms ps Identity
type MaxBounded        = Simple MaxBoundedQM Identity

type BoundedQM ms ps m idx cont = (MinBoundedQM ms ps m idx cont, MaxBoundedQM ms ps m idx cont)
type BoundedM        m idx cont = Simple MaxBoundedQM m idx cont
type BoundedQ  ms ps m idx cont = MaxBoundedQM ms ps Identity idx cont
type Bounded           idx cont = BoundedM Identity idx cont

-- utils

sizeM'     = queryBuilder sizeQM
size'      = withTransFunc (fmap2 runIdentity) sizeM'
sizeM      = queryBuilder $ fmap formatResult .: sizeQM
size       = withTransFunc (fmap2 runIdentity) sizeM

minBoundM' = queryBuilder minBoundQM
minBound'  = withTransFunc (fmap2 runIdentity) minBoundM'
minBoundM  = queryBuilder $ fmap formatResult .: minBoundQM
minBound   = withTransFunc (fmap2 runIdentity) minBoundM

maxBoundM' = queryBuilder maxBoundQM
maxBound'  = withTransFunc (fmap2 runIdentity) maxBoundM'
maxBoundM  = queryBuilder $ fmap formatResult .: maxBoundQM
maxBound   = withTransFunc (fmap2 runIdentity) maxBoundM




-- === Construction ===

-- Singleton
-- Allocable
-- Expandable
-- Growable

class Ctx ms m cont => SingletonQM  ms ps m el cont where singletonQM :: Query ms ps         -> el   -> m (ElResult   SingletonOp  ms el (ContainerOf cont) cont)
class Ctx ms m cont => ExpandableQM ms ps m    cont where expandQM    :: Query ms ps         -> cont -> m (PrimResult ExpandableOp ms    (ContainerOf cont) cont)
class Ctx ms m cont => AllocableQM  ms ps m    cont where allocQM     :: Query ms ps -> Int          -> m (PrimResult AllocableOp  ms    (ContainerOf cont) cont)
class Ctx ms m cont => GrowableQM   ms ps m    cont where growQM      :: Query ms ps -> Int  -> cont -> m (PrimResult GrowableOp   ms    (ContainerOf cont) cont)

data  SingletonOp       = SingletonOp
type  SingletonM        = Simple SingletonQM
type  SingletonQ  ms ps = SingletonQM ms ps Identity
type  Singleton         = Simple SingletonQM Identity

data  AllocableOp       = AllocableOp
type  AllocableM        = Simple AllocableQM
type  AllocableQ ms ps  = AllocableQM ms ps Identity
type  Allocable         = Simple AllocableQM Identity

data  ExpandableOp      = ExpandableOp
type  ExpandableM       = Simple ExpandableQM
type  ExpandableQ ms ps = ExpandableQM ms ps Identity
type  Expandable        = Simple ExpandableQM Identity

data  GrowableOp        = GrowableOp
type  GrowableM         = Simple GrowableQM
type  GrowableQ ms ps   = GrowableQM ms ps Identity
type  Growable          = Simple GrowableQM Identity

type instance IdxMod SingletonOp  a = a
type instance IdxMod AllocableOp  a = [a]
type instance IdxMod ExpandableOp a = [a]
type instance IdxMod GrowableOp   a = [a]

-- utils

singletonM' = queryBuilder singletonQM
singleton'  = withTransFunc (fmap2 runIdentity) singletonM'
singletonM  = queryBuilder $ fmap formatResult .: singletonQM
singleton   = withTransFunc (fmap2 runIdentity) singletonM

allocM'     = queryBuilder allocQM
alloc'      = withTransFunc (fmap2 runIdentity) allocM'
allocM      = queryBuilder $ fmap formatResult .: allocQM
alloc       = withTransFunc (fmap2 runIdentity) allocM

expandM'    = queryBuilder expandQM
expand'     = withTransFunc (fmap2 runIdentity) expandM'
expandM     = queryBuilder $ fmap formatResult .: expandQM
expand      = withTransFunc (fmap2 runIdentity) expandM

growM'      = queryBuilder growQM
grow'       = withTransFunc (fmap3 runIdentity) growM'
growM       = queryBuilder $ fmap formatResult .:. growQM
grow        = withTransFunc (fmap3 runIdentity) growM



-- === Modification ===
-- Appendable
-- Prependable
-- Addable
-- Removable
-- Insertable
-- Freeable



class Ctx ms m cont => AppendableQM   ms ps m     el cont where appendQM  :: Query ms ps        -> el -> cont -> m (ElResult    AppendableOp    ms     el (ContainerOf cont) cont)
class Ctx ms m cont => PrependableQM  ms ps m     el cont where prependQM :: Query ms ps        -> el -> cont -> m (ElResult    PrependableOp   ms     el (ContainerOf cont) cont)
class Ctx ms m cont => AddableQM      ms ps m     el cont where addQM     :: Query ms ps        -> el -> cont -> m (ElResult    AddableOp       ms     el (ContainerOf cont) cont)
class Ctx ms m cont => RemovableQM    ms ps m     el cont where removeQM  :: Query ms ps        -> el -> cont -> m (ElResult    RemovableOp     ms     el (ContainerOf cont) cont)
class Ctx ms m cont => InsertableQM   ms ps m idx el cont where insertQM  :: Query ms ps -> idx -> el -> cont -> m (IdxElResult InsertableOp    ms idx el (ContainerOf cont) cont)
class Ctx ms m cont => FreeableQM     ms ps m idx    cont where freeQM    :: Query ms ps -> idx       -> cont -> m (IdxResult   FreeableOp      ms idx    (ContainerOf cont) cont)

data  AppendableOp       = AppendableOp
type  AppendableM        = Simple AppendableQM
type  AppendableQ  ms ps = AppendableQM ms ps Identity
type  Appendable         = Simple AppendableQM Identity

data  PrependableOp      = PrependableOp
type  PrependableM       = Simple PrependableQM
type  PrependableQ ms ps = PrependableQM ms ps Identity
type  Prependable        = Simple PrependableQM Identity

data  AddableOp          = AddableOp
type  AddableM           = Simple AddableQM
type  AddableQ     ms ps = AddableQM ms ps Identity
type  Addable            = Simple AddableQM Identity

data  RemovableOp        = RemovableOp
type  RemovableM         = Simple RemovableQM
type  RemovableQ   ms ps = RemovableQM ms ps Identity
type  Removable          = Simple RemovableQM Identity

data  InsertableOp       = InsertableOp
type  InsertableM        = Simple InsertableQM
type  InsertableQ  ms ps = InsertableQM ms ps Identity
type  Insertable         = Simple InsertableQM Identity

data  FreeableOp         = FreeableOp
type  FreeableM          = Simple FreeableQM
type  FreeableQ    ms ps = FreeableQM ms ps Identity
type  Freeable           = Simple FreeableQM Identity

type instance IdxMod AppendableOp  a = a
type instance IdxMod PrependableOp a = a
type instance IdxMod AddableOp     a = a
type instance IdxMod RemovableOp   a = a
type instance IdxMod InsertableOp  a = a

appendM'  = queryBuilder appendQM
append'   = withTransFunc (fmap3 runIdentity) appendM'
appendM   = queryBuilder $ fmap formatResult .:. appendQM
append    = withTransFunc (fmap3 runIdentity) appendM

prependM' = queryBuilder prependQM
prepend'  = withTransFunc (fmap3 runIdentity) prependM'
prependM  = queryBuilder $ fmap formatResult .:. prependQM
prepend   = withTransFunc (fmap3 runIdentity) prependM

addM'     = queryBuilder addQM
add'      = withTransFunc (fmap3 runIdentity) addM'
addM      = queryBuilder $ fmap formatResult .:. addQM
add       = withTransFunc (fmap3 runIdentity) addM

removeM'  = queryBuilder removeQM
remove'   = withTransFunc (fmap3 runIdentity) removeM'
removeM   = queryBuilder $ fmap formatResult .:. removeQM
remove    = withTransFunc (fmap3 runIdentity) removeM

insertM'  = queryBuilder insertQM
insert'   = withTransFunc (fmap4 runIdentity) insertM'
insertM   = queryBuilder $ fmap formatResult .:: insertQM
insert    = withTransFunc (fmap4 runIdentity) insertM

freeM'    = queryBuilder freeQM
free'     = withTransFunc (fmap3 runIdentity) freeM'
freeM     = queryBuilder $ fmap formatResult .:. freeQM
free      = withTransFunc (fmap3 runIdentity) freeM



---- === Indexing ===

-- Indexable
-- TracksFreeIxes
-- TracksUsedIxes
-- TracksIxes
-- TracksElems

class Ctx ms m cont => IndexableQM      ms ps m idx el cont where indexQM    :: Query ms ps -> idx -> cont -> m (IdxElResult IndexableOp      ms idx el (ContainerOf cont) el   )
class Ctx ms m cont => TracksFreeIxesQM ms ps m idx    cont where freeIxesQM :: Query ms ps ->        cont -> m (IdxResult   TracksFreeIxesOp ms idx    (ContainerOf cont) [idx])
class Ctx ms m cont => TracksUsedIxesQM ms ps m idx    cont where usedIxesQM :: Query ms ps ->        cont -> m (IdxResult   TracksUsedIxesOp ms idx    (ContainerOf cont) [idx])
class Ctx ms m cont => TracksIxesQM     ms ps m idx    cont where ixesQM     :: Query ms ps ->        cont -> m (IdxResult   TracksIxesOp     ms idx    (ContainerOf cont) [idx])
class Ctx ms m cont => TracksElemsQM    ms ps m     el cont where elemsQM    :: Query ms ps ->        cont -> m (ElResult    TracksElemsOp    ms     el (ContainerOf cont) [el] )

data  IndexableOp            = IndexableOp
type  IndexableM             = Simple IndexableQM
type  IndexableQ       ms ps = IndexableQM ms ps Identity
type  Indexable              = Simple IndexableQM Identity

data  TracksFreeIxesOp       = TracksFreeIxesOp
type  TracksFreeIxesM        = Simple TracksFreeIxesQM
type  TracksFreeIxesQ  ms ps = TracksFreeIxesQM ms ps Identity
type  TracksFreeIxes         = Simple TracksFreeIxesQM Identity

data  TracksUsedIxesOp       = TracksUsedIxesOp
type  TracksUsedIxesM        = Simple TracksUsedIxesQM
type  TracksUsedIxesQ  ms ps = TracksUsedIxesQM ms ps Identity
type  TracksUsedIxes         = Simple TracksUsedIxesQM Identity

data  TracksIxesOp           = TracksIxesOp
type  TracksIxesM            = Simple TracksIxesQM
type  TracksIxesQ      ms ps = TracksIxesQM ms ps Identity
type  TracksIxes             = Simple TracksIxesQM Identity

data  TracksElemsOp          = TracksElemsOp
type  TracksElemsM           = Simple TracksElemsQM
type  TracksElemsQ     ms ps = TracksElemsQM ms ps Identity
type  TracksElems            = Simple TracksElemsQM Identity

type instance IdxMod IndexableOp   a = a
type instance IdxMod TracksElemsOp a = [a]

indexM'    = queryBuilder indexQM
index'     = withTransFunc (fmap3 runIdentity) indexM'
indexM     = queryBuilder $ fmap formatResult .:. indexQM
index      = withTransFunc (fmap3 runIdentity) indexM

freeIxesM' = queryBuilder freeIxesQM
freeIxes'  = withTransFunc (fmap2 runIdentity) freeIxesM'
freeIxesM  = queryBuilder $ fmap formatResult .: freeIxesQM
freeIxes   = withTransFunc (fmap2 runIdentity) freeIxesM

usedIxesM' = queryBuilder usedIxesQM
usedIxes'  = withTransFunc (fmap2 runIdentity) usedIxesM'
usedIxesM  = queryBuilder $ fmap formatResult .: usedIxesQM
usedIxes   = withTransFunc (fmap2 runIdentity) usedIxesM

ixesM'     = queryBuilder ixesQM
ixes'      = withTransFunc (fmap2 runIdentity) ixesM'
ixesM      = queryBuilder $ fmap formatResult .: ixesQM
ixes       = withTransFunc (fmap2 runIdentity) ixesM

elemsM'    = queryBuilder elemsQM
elems'     = withTransFunc (fmap2 runIdentity) elemsM'
elemsM     = queryBuilder $ fmap formatResult .: elemsQM
elems      = withTransFunc (fmap2 runIdentity) elemsM






type family Tup2RTup t where
    Tup2RTup ()               = () 
    Tup2RTup (t1, t2)         = (t1,(t2,())) 
    Tup2RTup (t1, t2, t3)     = (t1,(t2,(t3,()))) 
    Tup2RTup (t1, t2, t3, t4) = (t1,(t2,(t3,(t4,())))) 
    Tup2RTup a                = (a,())


resToRTup (Res ds a) = (a,ds)

formatResult = rtup2tupX . resToRTup

class rt ~ Tup2RTup t => RTup2TupX rt t | rt -> t where rtup2tupX :: rt -> t
instance {-# OVERLAPPABLE #-}                                      RTup2TupX () () where rtup2tupX = id
instance {-# OVERLAPPABLE #-} (Tup2RTup t1 ~ (t1,()), t1 ~ t1') => RTup2TupX (t1,()) t1' where rtup2tupX (t1,()) = t1
instance {-# OVERLAPPABLE #-} (t1 ~ t1', t2 ~ t2')              => RTup2TupX (t1,(t2,())) (t1',t2') where rtup2tupX (t1,(t2,())) = (t1,t2)
instance {-# OVERLAPPABLE #-} (t1 ~ t1', t2 ~ t2', t3 ~ t3')    => RTup2TupX (t1,(t2,(t3,()))) (t1',t2',t3') where rtup2tupX (t1,(t2,(t3,()))) = (t1,t2,t3)



type family PrettyCtx ms a :: Constraint where 
    PrettyCtx '[] a = Tup2RTup a ~ (a,())
    PrettyCtx ms  a = ()

type Ctx ms m cont = (Monad m, PrettyCtx ms cont)







ixed      = Opts.queryBuilder $ Opts.transFunc .: Opts.extendOptBuilder (Query :: Query '[ Opts.Ixed ] '[]                )
raw       = Opts.queryBuilder $ Opts.transFunc .: Opts.extendOptBuilder (Query :: Query '[]            '[ Opts.Raw       ])
try       = Opts.queryBuilder $ Opts.transFunc .: Opts.extendOptBuilder (Query :: Query '[]            '[ Opts.Try       ])
unchecked = Opts.queryBuilder $ Opts.transFunc .: Opts.extendOptBuilder (Query :: Query '[]            '[ Opts.Unchecked ])
unsafe    = Opts.queryBuilder $ Opts.transFunc .: Opts.extendOptBuilder (Query :: Query '[]            '[ Opts.Unsafe    ])