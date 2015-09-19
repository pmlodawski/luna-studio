{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE RankNTypes      #-}

{-# LANGUAGE PartialTypeSignatures #-}
--{-# LANGUAGE PolyKinds      #-}


module Data.Containers.Class where

import           Prologue        hiding (Indexable, index, Bounded, Ixed, Simple, Indexed)
import           Data.Maybe             (fromJust)
import           Data.Typeable

import           Data.Map    (Map)
import qualified Data.Map    as Map
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Data.TypeLevel.List (In)

import Data.TypeLevel.Bool

import Data.Containers.Poly
import GHC.Prim


type Unique lst = Reverse (Unique' lst '[])

type family Unique' lst reg where
  Unique' '[]       reg = reg
  Unique' (l ': ls) reg = Unique' ls (If (l `In` reg) reg (l ': reg))

type Reverse lst = Reverse' lst '[]

type family Reverse' lst lst' where
  Reverse' '[]       lst = lst
  Reverse' (l ': ls) lst = Reverse' ls (l ': lst)

uniqueProxy :: Proxy a -> Proxy (Unique a)
uniqueProxy _ = Proxy

filterMutable :: Proxy a -> Proxy (FilterMutable a)
filterMutable _ = Proxy

--- === Bounded ===


--class (Ord idx, Enum idx) => Bounded cont idx where
--    minIndexIdx :: cont -> idx
--    maxBoundIdx :: cont -> idx

--minIdx :: Bounded cont (IndexOf' cont) => cont -> IndexOf' cont
--minIdx = minIndexIdx

--maxIdx :: Bounded cont (IndexOf' cont) => cont -> IndexOf' cont
--maxIdx = maxBoundIdx

--- === HasContainer ===

class HasContainer a cont | a -> cont where
    container :: Lens' a cont


instance HasContainer [a]           [a]           where container = id

--type IsContainer a = HasContainer a a

class HasContainer2 a => IsContainer a where
    fromContainer :: ContainerOf a -> a

--- === Containers ===



--class Measurable a where sizeQSM :: Integral i => a -> i

-- GHC BUG [WD]: we can removeQSM fundeps here when it will be fixed: https://ghc.haskell.org/trac/ghc/ticket/10778
class (IndexOf el cont ~ idx, ElementByIx idx cont ~ el) => Container cont idx el | el cont -> idx, idx cont -> el where
    --eleQSm   :: cont -> [el]
    --indexes :: cont -> [idx]


---- === Finite ===
--class Measurable     q m cont        sizeQSM  | q m cont        -> sizeQSM  where sizeQSM      :: InstModsX Measurable      q m cont ->              cont -> sizeQSM
--class MinIndexed     q m cont        idx   | q m cont        -> idx   where minIndexQSM  :: InstModsX MinIndexed      q m cont ->              cont -> idx
--class MaxIndexed     q m cont        idx   | q m cont        -> idx   where maxIndexQSM  :: InstModsX MaxIndexed      q m cont ->              cont -> idx

---- === Construction ===
--class Singleton      q m cont     el                                  where singletonQSM :: InstModsX Singleton       q m cont ->        el -> cont
--class Allocable      q m cont                                         where allocQSM     :: InstModsX Allocable       q m cont -> Int ->       cont
--class Growable       q m cont        cont' | q m cont        -> cont' where growQSM      :: InstModsX Growable        q m cont -> Int ->       cont -> cont'
--class Expandable     q m cont        cont' | q m cont        -> cont' where expandQSM    :: InstModsX Expandable      q m cont ->              cont -> cont'


--class ExpandableF    q s cont m        where expandF    :: Monad m => Query q s -> ExpandableInfo cont ->               cont -> m (ResultByQuery q (ExpandableInfo cont) cont)

--type ResultByQuery' = ResultByQuery (Selected s (ModsOf cont Expandable)) (ExpandableInfo cont) cont
--type ResultByQuery' info s = ResultByQuery (Selected s (ModsOf (InfoCont info) (InfoCls info))) (ExpandableInfo (InfoCont info)) (InfoCont info)
--type family ResultByQuery' info s :: * -> * where ResultByQuery' (Info idx el cls cont) s = ResultByQuery (Selected s (ModsOf cont cls)) (cls cont)

--class AppendableX    q m cont     el where appendx  :: InstModsX AppendableX      q m cont ->        el -> cont -> MonoResultEl q cont el
--class Singleton      q m cont     el                                  where singletonQSM :: InstModsX Singleton       q m cont ->        el -> cont


--type Expandable2Info    = RawInfo Expandable2



type ExpandableFinal    = OperationBase ExpandableInfo
type ExpandableFinalT q = Operation2 q  ExpandableInfo

type MaxIndexedFinal    = OperationBase MaxIndexedInfo
type MaxIndexedFinalT q = Operation2 q  MaxIndexedInfo

type MinIndexedFinal    = OperationBase MinIndexedInfo
type MinIndexedFinalT q = Operation2 q  MinIndexedInfo

type AppendableFinal    el = OperationBase (AppendableInfo el)
type AppendableFinalT q el = Operation2 q  (AppendableInfo el)

type PrependableFinal    el = OperationBase (PrependableInfo el)
type PrependableFinalT q el = Operation2 q  (PrependableInfo el)

type AddableFinal    el = OperationBase (AddableInfo el)
type AddableFinalT q el = Operation2 q  (AddableInfo el)

type RemovableFinal    el = OperationBase (RemovableInfo el)
type RemovableFinalT q el = Operation2 q  (RemovableInfo el)

type SingletonFinal    el = OperationBase (SingletonInfo el)
type SingletonFinalT q el = Operation2 q  (SingletonInfo el)

type AllocableFinal    = OperationBase AllocableInfo
type AllocableFinalT q = Operation2 q  AllocableInfo

type MeasurableFinal      = OperationBase MeasurableInfo

type GrowableFinal    = OperationBase GrowableInfo
type GrowableFinalT q = Operation2 q  GrowableInfo


data Impossible    = Impossible  deriving (Show)
data ImpossibleM a = ImpossibleM deriving (Show, Functor)


--instance Meas
    --MeasurableFinalT q m t => Proxy q -> t -> m (ResultByQuery (MeasurableInfo (ContainerOf t)) q Int)


type OpCtx i q (m :: * -> *) t = ( IsDataStore    t
                                 , TransDataStore t
                                 , AssertQuery (i (DataStoreOf t)) q
                                 , SuperInst   (i (DataStoreOf t)) q m
                                 , Functor m
                                 , Functor (ResultByQuery (i (DataStoreOf t)) q)
                                 )

type TransDataStore t = DataStoreOf (DataStoreOf t) ~ DataStoreOf t

type AssertQuery i q = CheckQuery i q (InfoSelection i q)

type AssertQuery' i q = CheckQuery (DataStoreInfo i) q (InfoSelection i q)


type TransDataStore2 t = DataStoreOf (ContainerOf t) ~ DataStoreOf t



type OpCtx2 i q (m :: * -> *) t = ( SuperInst (i (ContainerOf t)) q m
                                  , AssertQuery2 (i (ContainerOf t)) q
                                  , IsContainer t
                                  , Functor m
                                  , Functor (ResultBySel' (i (ContainerOf t)) (InfoSelection (i (ContainerOf t)) q))
                                  , TransDataStore2 t
                                  )


impossible = error "Impossible happened."

type Result''   info q t = ResultByQuery (info (DataStoreOf (ContainerOf t))) q


-- === Finite ===


class MeasurableQSM cont m q s where sizeQSM      :: (CheckQuery' info q s, info ~ MeasurableInfo cont) => Query q s -> info -> cont -> m (ResultBySel' info s Int)
class MinIndexedQSM cont m q s where minIndexQSM  :: (CheckQuery' info q s, info ~ MinIndexedInfo cont) => Query q s -> info -> cont -> m (ResultBySel' info s (IndexOf' (DataStoreOf cont)))
class MaxIndexedQSM cont m q s where maxIndexQSM  :: (CheckQuery' info q s, info ~ MaxIndexedInfo cont) => Query q s -> info -> cont -> m (ResultBySel' info s (IndexOf' (DataStoreOf cont)))

class                         OpCtx2 MeasurableInfo q m t           => MeasurableQM q m t           where sizeQM :: Proxy q -> t -> m (Result'' MeasurableInfo q t Int)
instance {-# OVERLAPPABLE #-} OpCtx2 MeasurableInfo q m t           => MeasurableQM q m t           where sizeQM q = runModsF' sizeQSM q . view container2
instance {-# OVERLAPPABLE #-} OpCtx2 MeasurableInfo q ImpossibleM t => MeasurableQM q ImpossibleM t where sizeQM   = impossible
instance {-# OVERLAPPABLE #-} OpCtx2 MeasurableInfo q m Impossible  => MeasurableQM q m Impossible  where sizeQM   = impossible
type MeasurableInfo = RawInfo MeasurableQSM
type MeasurableQ q  = MeasurableQM q Identity
type MeasurableM    = MeasurableQM '[]
type Measurable     = MeasurableM Identity

class                         OpCtx2 MinIndexedInfo q m t           => MinIndexedQM q m t           where minIndexQM :: Proxy q -> t -> m (Result'' MinIndexedInfo q t (IndexOf' (DataStoreOf t)))
instance {-# OVERLAPPABLE #-} OpCtx2 MinIndexedInfo q m t           => MinIndexedQM q m t           where minIndexQM q = runModsF' minIndexQSM q . view container2
instance {-# OVERLAPPABLE #-} OpCtx2 MinIndexedInfo q m Impossible  => MinIndexedQM q m Impossible  where minIndexQM   = impossible
instance {-# OVERLAPPABLE #-} OpCtx2 MinIndexedInfo q ImpossibleM t => MinIndexedQM q ImpossibleM t where minIndexQM   = impossible
type MinIndexedInfo = RawInfo MinIndexedQSM
type MinIndexedQ  q = MinIndexedQM q Identity
type MinIndexedM    = MinIndexedQM '[]
type MinIndexed     = MinIndexedM Identity

class                         OpCtx2 MaxIndexedInfo q m t           => MaxIndexedQM q m t           where maxIndexQM :: Proxy q -> t -> m (Result'' MaxIndexedInfo q t (IndexOf' (DataStoreOf t)))
instance {-# OVERLAPPABLE #-} OpCtx2 MaxIndexedInfo q m t           => MaxIndexedQM q m t           where maxIndexQM q = runModsF' maxIndexQSM q . view container2
instance {-# OVERLAPPABLE #-} OpCtx2 MaxIndexedInfo q m Impossible  => MaxIndexedQM q m Impossible  where maxIndexQM   = impossible
instance {-# OVERLAPPABLE #-} OpCtx2 MaxIndexedInfo q ImpossibleM t => MaxIndexedQM q ImpossibleM t where maxIndexQM   = impossible
type MaxIndexedInfo = RawInfo MaxIndexedQSM
type MaxIndexedQ  q = MaxIndexedQM q Identity
type MaxIndexedM    = MaxIndexedQM '[]
type MaxIndexed     = MaxIndexedM Identity

-- size

sizeM' :: MeasurableQM q m t => Func' q (t -> m (Result'' MeasurableInfo q t Int))
sizeM' = transFunc $ optBuilder sizeQM

sizeM :: (MeasurableQM q m t, Simplified'' MeasurableInfo q t Int out, Functor m) => Func' q (t -> m out)
sizeM = withTransFunc (fmap3 simplify) sizeM'

size :: (MeasurableQ q t, Simplified'' MeasurableInfo q t Int out) => Func' q (t -> out)
size = withTransFunc (fmap2 runIdentity) sizeM

-- minIndex

minIndexM' :: MinIndexedQM q m t => Func' q (t -> m (Result'' MinIndexedInfo q t (IndexOf' (DataStoreOf t))))
minIndexM' = transFunc $ optBuilder minIndexQM

minIndexM :: (MinIndexedQM q m t, Simplified'' MinIndexedInfo q t (IndexOf' (DataStoreOf t)) out, Functor m) => Func' q (t -> m out)
minIndexM = withTransFunc (fmap3 simplify) minIndexM'

minIndex :: (MinIndexedQM q Identity t, Simplified'' MinIndexedInfo q t (IndexOf' (DataStoreOf t)) out) => Func' q (t -> out)
minIndex = withTransFunc (fmap2 runIdentity) minIndexM

-- maxIndex

maxIndexM' :: MaxIndexedQM q m t => Func' q (t -> m (Result'' MaxIndexedInfo q t (IndexOf' (DataStoreOf t))))
maxIndexM' = transFunc $ optBuilder maxIndexQM

maxIndexM :: (MaxIndexedQM q m t, Simplified'' MaxIndexedInfo q t (IndexOf' (DataStoreOf t)) out, Functor m) => Func' q (t -> m out)
maxIndexM = withTransFunc (fmap3 simplify) maxIndexM'

maxIndex :: (MaxIndexedQM q Identity t, Simplified'' MaxIndexedInfo q t (IndexOf' (DataStoreOf t)) out) => Func' q (t -> out)
maxIndex = withTransFunc (fmap2 runIdentity) maxIndexM




--(FilterMutable (ModsOf ExpandableQSM2 cont)))


--type MyResult info q t = ResultX (info (DataStoreOf (ContainerOf t))) q
type MyResult info q t = ResultX (info (DataStoreOf (ContainerOf t))) (FilterMutable q)



includeResult :: Proxy q -> Proxy (AppendLst R q)
includeResult _ = Proxy


--s ~ ComputeSelection cls cont q


foo (q :: Proxy (q :: [*])) (tc :: cont) = out where
    tc2 = runModsF' expandQSM2 (uniqueProxy (filterMutable q)) tc
    tc3 = withFlipped (fmap $ taggedCont (Proxy :: Proxy (Selected (ComputeSelection ExpandableQSM2 cont q) (FilterMutable (ModsOf ExpandableQSM2 cont))) )) <$> tc2
    out = tc3


--bar :: ( Functor m
--       , HasContainer2 t
--       , SuperInstR info q m
--       , MapResult (ContainerOf t) t (TaggedResult cls (ContainerOf t) (ContRes info q t))
--       , info     ~ ExpandableInfo2 (ContainerOf t)

--       , cls      ~ ExpandableQSM2

--       , DataFillable (WithResult q) (MappedResult t cls (ContRes info q t))
--       , Taggable (ResultMods cls (ContainerOf t)) (ContRes info q t)
--       , CheckFilledResult2 info q t (ContRes info q t)
--       ) => Proxy q -> t -> m (MyResult ExpandableInfo2 q t t)


--type Res info q cont = ResultBySelX info (InfoSelectionR info q) cont


--type ContRes info q t = Res info q (ContainerOf t)
--type CheckFilledMappedResult =

--type MappedInfoResul

type SuperInstR info q (m :: * -> *) = (SuperInst info (WithResult q) m :: Constraint)

type MapResult = MapByTag R


newtype Flipped t a b = Flipped { fromFlipped :: t b a } deriving Show

instance Functor (Flipped ResW r) where fmap f (Flipped (ResW d r)) = Flipped (ResW (f d) r)


withFlipped f = fromFlipped . f . Flipped

type CTXO q m t = ((Functor m,
                                               HasContainer2 t,
                                               DataFillable
                                                 q
                                                 (TaggedCont
                                                    (Selected
                                                       (LstIn
                                                          (ModsOf ExpandableQSM2 (ContainerOf t)) q)
                                                       (FilterMutable
                                                          (ModsOf ExpandableQSM2 (ContainerOf t))))
                                                    (ResultX
                                                       (Info
                                                          NA
                                                          NA
                                                          ExpandableQSM2
                                                          (DataStoreOf (ContainerOf t)))
                                                       (Selected
                                                          (LstIn
                                                             (ModsOf ExpandableQSM2 (ContainerOf t))
                                                             (Reverse'
                                                                (Unique' (FilterMutable q) '[])
                                                                '[]))
                                                          (FilterMutable
                                                             (ModsOf
                                                                ExpandableQSM2 (ContainerOf t)))))),
                                               Taggable
                                                 (Selected
                                                    (LstIn
                                                       (ModsOf ExpandableQSM2 (ContainerOf t)) q)
                                                    (FilterMutable
                                                       (ModsOf ExpandableQSM2 (ContainerOf t))))
                                                 (ResultX
                                                    (Info
                                                       NA
                                                       NA
                                                       ExpandableQSM2
                                                       (DataStoreOf (ContainerOf t)))
                                                    (Selected
                                                       (LstIn
                                                          (ModsOf ExpandableQSM2 (ContainerOf t))
                                                          (Reverse'
                                                             (Unique' (FilterMutable q) '[]) '[]))
                                                       (FilterMutable
                                                          (ModsOf
                                                             ExpandableQSM2 (ContainerOf t))))),
                                               ExpandableQSM2
                                                 (ContainerOf t)
                                                 m
                                                 (Reverse' (Unique' (FilterMutable q) '[]) '[])
                                                 (LstIn
                                                    (ModsOf ExpandableQSM2 (ContainerOf t))
                                                    (Reverse' (Unique' (FilterMutable q) '[]) '[])),
                                               ResultX
                                                 (Info
                                                    NA
                                                    NA
                                                    ExpandableQSM2
                                                    (DataStoreOf (ContainerOf t)))
                                                 (Reverse' (Unique' (FilterMutable q) '[]) '[])
                                               ~ ResultX
                                                   (Info
                                                      NA
                                                      NA
                                                      ExpandableQSM2
                                                      (DataStoreOf (ContainerOf t)))
                                                   (Selected
                                                      (LstIn
                                                         (ModsOf ExpandableQSM2 (ContainerOf t))
                                                         (Reverse'
                                                            (Unique' (FilterMutable q) '[]) '[]))
                                                      (FilterMutable
                                                         (ModsOf ExpandableQSM2 (ContainerOf t)))),
                                               ResultX
                                                 (ExpandableInfo2 (DataStoreOf (ContainerOf t)))
                                                 (FilterMutable q)
                                               ~ FillData
                                                   q
                                                   (TaggedCont
                                                      (Selected
                                                         (LstIn
                                                            (ModsOf ExpandableQSM2 (ContainerOf t))
                                                            q)
                                                         (FilterMutable
                                                            (ModsOf
                                                               ExpandableQSM2 (ContainerOf t))))
                                                      (ResultX
                                                         (Info
                                                            NA
                                                            NA
                                                            ExpandableQSM2
                                                            (DataStoreOf (ContainerOf t)))
                                                         (Selected
                                                            (LstIn
                                                               (ModsOf
                                                                  ExpandableQSM2 (ContainerOf t))
                                                               (Reverse'
                                                                  (Unique' (FilterMutable q) '[])
                                                                  '[]))
                                                            (FilterMutable
                                                               (ModsOf
                                                                  ExpandableQSM2
                                                                  (ContainerOf t))))))),

                                                                  -- manual
                                                                  DataStoreOf (ContainerOf t) ~ DataStoreOf t,
                                                                  (ResultX (ExpandableInfo2 (DataStoreOf t)) (FilterMutable q) ~ ResultX (ExpandableInfo2 (DataStoreOf t)) (FilterMutable q)))

bar :: CTXO q m t => Proxy q -> t -> m (ResW (MyResult ExpandableInfo2 q t) t)
bar q t = out where
    cont  = view container2 t
    tgdr  = foo q cont
    tgdr' = (fmap . fmap) (\c -> t & container2 .~ c) tgdr
    out   = withFlipped (fmap (fillData q)) <$> tgdr'



--bar2 :: _ => Proxy q -> t -> m (ResW (MyResult2 ExpandableInfo2 q t) t)
--bar2 q t = out where
--    cont  = view container2 t
--    tgdr  = foo q cont
--    tgdr' = (fmap . fmap) (\c -> t & container2 .~ c) tgdr
--    out   = withFlipped (fmap (fillData q)) <$> tgdr'



data ResW d r = ResW d r deriving (Show, Functor)

withResData :: (d -> (out, d')) -> ResW d r -> (out, ResW d' r)
withResData f (ResW d r) = (out, ResW d' r) where
  (out, d') = f d

withResData_ :: (d -> d') -> ResW d r -> ResW d' r
withResData_ = flattenMod withResData

splitResData :: ResW (d,ds) r -> (d, ResW ds r)
splitResData = withResData id

flattenMod :: Functor f => (f ((), a) -> b -> (x, c)) -> f a -> b -> c
flattenMod f = snd .: (f . fmap ((),))

--bar :: _ => Proxy q -> t -> m (MyResult ExpandableInfo q t t)
--bar q t = t' where
--    cont = view container2 t
--    r    = foo q cont
--    t'   = fmap (fmapRTup $ \x -> (t & container2 .~ x)) r


-- === Construction ===

type family MappedRTup a tup where
    MappedRTup a (t, ()) = (a, ())
    MappedRTup a (t, ts) = (t, MappedRTup a ts)

class                                                                                              RTupFunctor a b tup    where fmapRTup :: (a -> b) -> tup -> MappedRTup b tup
instance {-# OVERLAPPABLE #-} (a ~ t)                                                           => RTupFunctor a b (t,()) where fmapRTup f (t,()) = (f t,())
instance {-# OVERLAPPABLE #-} (RTupFunctor a b ts, MappedRTup b (t, ts) ~ (t, MappedRTup b ts)) => RTupFunctor a b (t,ts) where fmapRTup f (t,ts) = (t, fmapRTup f ts)


type family AppendedRTup a rt where
    AppendedRTup a ()     = (a,())
    AppendedRTup a (t,ts) = (t,AppendedRTup a ts)

class                       AppendRTup a rt     where appendRTup :: a -> rt -> AppendedRTup a rt
instance                    AppendRTup a ()     where appendRTup a _      = (a,())
instance AppendRTup a ts => AppendRTup a (t,ts) where appendRTup a (t,ts) = (t,appendRTup a ts)


type family   AsTup a
type instance AsTup () = ()
type instance AsTup (t1,()) = t1
type instance AsTup (t1,(t2,())) = (t1,t2)
type instance AsTup (t1,(t2,(t3,()))) = (t1,t2,t3)
type instance AsTup (t1,(t2,(t3,(t4,())))) = (t1,t2,t3,t4)
type instance AsTup (t1,(t2,(t3,(t4,(t5,()))))) = (t1,t2,t3,t4,t5)
type instance AsTup (t1,(t2,(t3,(t4,(t5,(t6,())))))) = (t1,t2,t3,t4,t5,t6)
type instance AsTup (t1,(t2,(t3,(t4,(t5,(t6,(t7,()))))))) = (t1,t2,t3,t4,t5,t6,t7)
type instance AsTup (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,())))))))) = (t1,t2,t3,t4,t5,t6,t7,t8)
type instance AsTup (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,()))))))))) = (t1,t2,t3,t4,t5,t6,t7,t8,t9)

class    ToTup a  where toTup :: a -> AsTup a
instance ToTup () where toTup _ = ()
instance ToTup (t1,()) where toTup (t1,()) = t1
instance ToTup (t1,(t2,())) where toTup (t1,(t2,())) = (t1,t2)
instance ToTup (t1,(t2,(t3,()))) where toTup (t1,(t2,(t3,()))) = (t1,t2,t3)
instance ToTup (t1,(t2,(t3,(t4,())))) where toTup (t1,(t2,(t3,(t4,())))) = (t1,t2,t3,t4)
instance ToTup (t1,(t2,(t3,(t4,(t5,()))))) where toTup (t1,(t2,(t3,(t4,(t5,()))))) = (t1,t2,t3,t4,t5)
instance ToTup (t1,(t2,(t3,(t4,(t5,(t6,())))))) where toTup (t1,(t2,(t3,(t4,(t5,(t6,())))))) = (t1,t2,t3,t4,t5,t6)
instance ToTup (t1,(t2,(t3,(t4,(t5,(t6,(t7,()))))))) where toTup (t1,(t2,(t3,(t4,(t5,(t6,(t7,()))))))) = (t1,t2,t3,t4,t5,t6,t7)
instance ToTup (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,())))))))) where toTup (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,())))))))) = (t1,t2,t3,t4,t5,t6,t7,t8)
instance ToTup (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,()))))))))) where toTup (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,()))))))))) = (t1,t2,t3,t4,t5,t6,t7,t8,t9)



class ExpandableQSM2            cont m q s where expandQSM2   :: (AssumeQuery info q s, info ~ ExpandableInfo2  cont) => Query q s -> info ->        cont -> m (ResW (ResultBySelX info s) cont)
--ComputeSelection cls cont q
class                                                  CTXO q m t           => ExpandableQM2 q m t           where expandQM2 :: Proxy q -> t -> m (ResW (MyResult ExpandableInfo2 q t) t)
instance {-# OVERLAPPABLE #-}                          CTXO q m t           => ExpandableQM2 q m t           where expandQM2 = bar
instance {-# OVERLAPPABLE #-}                          CTXO q m Impossible  => ExpandableQM2 q m Impossible  where expandQM2 = impossible
instance {-# OVERLAPPABLE #-}                          CTXO q ImpossibleM t => ExpandableQM2 q ImpossibleM t where expandQM2 = impossible


type instance IxedMode ExpandableQSM2 = Multi

type ExpandableInfo2 = RawInfo ExpandableQSM2
--type ExpandableQ  q = ExpandableQM q Identity
--type ExpandableM    = ExpandableQM '[]
--type Expandable     = ExpandableM Identity




class SingletonQSM           el cont m q s where singletonQSM :: (CheckQuery' info q s, info ~ SingletonInfo el cont) => Query q s -> info -> el          -> m (ResultBySel' info s cont)
class AllocableQSM              cont m q s where allocQSM     :: (CheckQuery' info q s, info ~ AllocableInfo    cont) => Query q s -> info -> Int         -> m (ResultBySel' info s cont)
class ExpandableQSM             cont m q s where expandQSM    :: (CheckQuery' info q s, info ~ ExpandableInfo   cont) => Query q s -> info ->        cont -> m (ResultBySel' info s cont)
class GrowableQSM               cont m q s where growQSM      :: (CheckQuery' info q s, info ~ GrowableInfo     cont) => Query q s -> info -> Int -> cont -> m (ResultBySel' info s cont)

type instance IxedMode SingletonQSM  = Single
type instance IxedMode AllocableQSM  = Multi
type instance IxedMode ExpandableQSM = Multi
type instance IxedMode GrowableQSM   = Multi

class                         OpCtx2 (SingletonInfo el) q m t           => SingletonQM el q m t           where singletonQM :: Proxy q -> el -> m (Result'' (SingletonInfo el) q t t)
instance {-# OVERLAPPABLE #-} OpCtx2 (SingletonInfo el) q m t           => SingletonQM el q m t           where singletonQM = (fmap . fmap) fromContainer .: runModsF' singletonQSM
instance {-# OVERLAPPABLE #-} OpCtx2 (SingletonInfo el) q m Impossible  => SingletonQM el q m Impossible  where singletonQM = impossible
instance {-# OVERLAPPABLE #-} OpCtx2 (SingletonInfo el) q ImpossibleM t => SingletonQM el q ImpossibleM t where singletonQM = impossible
type SingletonInfo el   = ElInfo el SingletonQSM
type SingletonQ    el q = SingletonQM el q Identity
type SingletonM    el   = SingletonQM el '[]
type Singleton     el   = SingletonM  el Identity

class                         OpCtx2 AllocableInfo q m t           => AllocableQM q m t           where allocQM :: Proxy q -> Int -> m (Result'' AllocableInfo q t t)
instance {-# OVERLAPPABLE #-} OpCtx2 AllocableInfo q m t           => AllocableQM q m t           where allocQM = (fmap . fmap) fromContainer .: runModsF' allocQSM
instance {-# OVERLAPPABLE #-} OpCtx2 AllocableInfo q m Impossible  => AllocableQM q m Impossible  where allocQM = impossible
instance {-# OVERLAPPABLE #-} OpCtx2 AllocableInfo q ImpossibleM t => AllocableQM q ImpossibleM t where allocQM = impossible
type AllocableInfo = RawInfo AllocableQSM
type AllocableQ  q = AllocableQM q Identity
type AllocableM    = AllocableQM '[]
type Allocable     = AllocableM Identity

class                         OpCtx2 ExpandableInfo q m t           => ExpandableQM q m t           where expandQM :: Proxy q -> t -> m (Result'' ExpandableInfo q t t)
instance {-# OVERLAPPABLE #-} OpCtx2 ExpandableInfo q m t           => ExpandableQM q m t           where expandQM q = nestedLens container2 (runModsF' expandQSM q)
instance {-# OVERLAPPABLE #-} OpCtx2 ExpandableInfo q m Impossible  => ExpandableQM q m Impossible  where expandQM   = impossible
instance {-# OVERLAPPABLE #-} OpCtx2 ExpandableInfo q ImpossibleM t => ExpandableQM q ImpossibleM t where expandQM   = impossible
type ExpandableInfo = RawInfo ExpandableQSM
type ExpandableQ  q = ExpandableQM q Identity
type ExpandableM    = ExpandableQM '[]
type Expandable     = ExpandableM Identity



type GrowableInfo      = RawInfo    GrowableQSM
--type ExpandableInfo    = RawInfo    ExpandableQSM


-- singleton

singletonM' :: SingletonQM el q m t => Func' q (el -> m (Result'' (SingletonInfo el) q t t))
singletonM' = transFunc $ optBuilder singletonQM

singletonM :: (SingletonQM el q m t, Simplified'' (SingletonInfo el) q t t out, Functor m) => Func' q (el -> m out)
singletonM = withTransFunc (fmap3 simplify) singletonM'

singleton :: (SingletonQ el q t, Simplified'' (SingletonInfo el) q t t out) => Func' q (el -> out)
singleton = withTransFunc (fmap2 runIdentity) singletonM

-- allocable

allocM' :: AllocableQM q m t => Func' q (Int -> m (Result'' AllocableInfo q t t))
allocM' = transFunc $ optBuilder allocQM

allocM :: (AllocableQM q m t, Simplified'' AllocableInfo q t t out, Functor m) => Func' q (Int -> m out)
allocM = withTransFunc (fmap3 simplify) allocM'

alloc :: (AllocableQ q t, Simplified'' AllocableInfo q t t out) => Func' q (Int -> out)
alloc = withTransFunc (fmap2 runIdentity) allocM

-- expandable

expandM' :: ExpandableQM q m t => Func' q (t -> m (Result'' ExpandableInfo q t t))
expandM' = transFunc $ optBuilder expandQM

expandM :: (ExpandableQM q m t, Simplified'' ExpandableInfo q t t out, Functor m) => Func' q (t -> m out)
expandM = withTransFunc (fmap3 simplify) expandM'

expand :: (ExpandableQ q t, Simplified'' ExpandableInfo q t t out) => Func' q (t -> out)
expand = withTransFunc (fmap2 runIdentity) expandM

-- expandable2

--expandM' :: ExpandableQM q m t => Func' q (t -> m (Result'' ExpandableInfo q t t))
expandM2' = transFunc $ optBuilder expandQM2

--expandM :: (ExpandableQM q m t, Simplified'' ExpandableInfo q t t out, Functor m) => Func' q (t -> m out)
idsimplify = id
expandM2 = withTransFunc (fmap3 simplify2) expandM2'

--expand :: (ExpandableQ q t, Simplified'' ExpandableInfo q t t out) => Func' q (t -> out)
expand2  = withTransFunc (fmap2 runIdentity) expandM2
expand2' = withTransFunc (fmap2 runIdentity) expandM2'


simplify2 (ResW d r) = toTup $ appendRTup r d

-- === Concatenation ===
class AppendableQSM          el cont m q s where appendQSM    :: info ~ AppendableInfo  el cont => Query q s -> info -> el -> cont -> m (ResultBySel info s cont)
class PrependableQSM         el cont m q s where prependQSM   :: info ~ PrependableInfo el cont => Query q s -> info -> el -> cont -> m (ResultBySel info s cont)
class AddableQSM             el cont m q s where addQSM       :: info ~ AddableInfo     el cont => Query q s -> info -> el -> cont -> m (ResultBySel info s cont)
class RemovableQSM           el cont m q s where removeQSM    :: info ~ RemovableInfo   el cont => Query q s -> info -> el -> cont -> m (ResultBySel info s cont)

type AppendableInfo  el = ElInfo el AppendableQSM
type PrependableInfo el = ElInfo el PrependableQSM
type AddableInfo     el = ElInfo el AddableQSM
type RemovableInfo   el = ElInfo el RemovableQSM

type instance IxedMode AppendableQSM  = Single
type instance IxedMode PrependableQSM = Single
type instance IxedMode AddableQSM     = Single
type instance IxedMode RemovableQSM   = Single


-- to remove, Measurable does not have Ixed! just a test
type instance IxedMode MeasurableQSM   = Single


type Result     info q t = ResultByQuery (info (ContainerOf t)) q
type Result'    info q t = ResultByQuery (info (DataStoreOf t)) q
type Simplified info q t = Simplify (Result info q t)
type Simplified' info q t = Simplify (Result' info q t)
type Simplified'' info q t = Simplify (Result'' info q t)






--xxx :: IxedX Measurable t => t -> (IndexOf' (ContainerOf t), Int)
--xxx q = ixed size q

--

--singletonM :: SingletonFinalT q el m t => Func' q (el -> m (ResultByQuery (SingletonInfo el (ContainerOf t)) q t))
--singletonM = transFunc $ optBuilder $ \q el -> (fmap . fmap) fromContainer $ runModsF' singletonQSM q el

--singletonM :: SingletonFinalT q el m t => Func' q (el -> m (ResultByQuery (SingletonInfo el (ContainerOf t)) q t))
--singletonM = transFunc $ optBuilder $ \q el -> (fmap . fmap) fromContainer $ runModsF' singletonQSM q el

--allocM :: AllocableFinalT q m t => Func' q (Int -> m (ResultByQuery (AllocableInfo (ContainerOf t)) q t))
--allocM = transFunc $ optBuilder $ \q i -> (fmap . fmap) fromContainer $ runModsF' allocQSM q i

--expandM :: ExpandableFinalT q m t => Func' q (t -> m (ResultByQuery (ExpandableInfo (ContainerOf t)) q t))
--expandM = transFunc $ optBuilder $ \q -> nestedLens container2 (runModsF' expandQSM q)

--growM :: GrowableFinalT q m t => Func' q (Int -> t -> m (ResultByQuery (GrowableInfo (ContainerOf t)) q t))
--growM = transFunc $ optBuilder $ \q i -> nestedLens container2 (runModsF' growQSM q i)

--

appendM :: AppendableFinalT q el m t => Func' q (el -> t -> m (ResultByQuery (AppendableInfo el (ContainerOf t)) q t))
appendM = transFunc $ optBuilder $ \q el -> nestedLens container2 (runModsF' appendQSM q el)

prependM :: PrependableFinalT q el m t => Func' q (el -> t -> m (ResultByQuery (PrependableInfo el (ContainerOf t)) q t))
prependM = transFunc $ optBuilder $ \q el -> nestedLens container2 (runModsF' prependQSM q el)

addM :: AddableFinalT q el m t => Func' q (el -> t -> m (ResultByQuery (AddableInfo el (ContainerOf t)) q t))
addM = transFunc $ optBuilder $ \q el -> nestedLens container2 (runModsF' addQSM q el)

removeM :: RemovableFinalT q el m t => Func' q (el -> t -> m (ResultByQuery (RemovableInfo el (ContainerOf t)) q t))
removeM = transFunc $ optBuilder $ \q el -> nestedLens container2 (runModsF' removeQSM q el)


--expandM2 :: _ => _
--maxIndexM  = withTransFunc (fmap3 simplify) maxIndexM'
--minIndexM  = withTransFunc (fmap3 simplify) minIndexM'

--singletonM2 = withTransFunc (fmap3 simplify) singletonM
--allocM2     = withTransFunc (fmap3 simplify) allocM
--expandM2    = withTransFunc (fmap3 simplify) expandM
--growM2      = withTransFunc (fmap4 simplify) growM

appendM2    = withTransFunc (fmap4 simplify) appendM
prependM2   = withTransFunc (fmap4 simplify) prependM
addM2       = withTransFunc (fmap4 simplify) addM
removeM2    = withTransFunc (fmap4 simplify) removeM






--class Expandable2              cont m q s w | cont s -> w where expand2    :: info ~ Expandable2Info    cont => Query q s -> info ->       cont -> m (w cont)

--class Expandable               cont m q s where expandQSM    :: info ~ ExpandableInfo    cont => Query q s -> info ->       cont -> m (ResultBySel info s cont)


--class Allocable      q m cont                                         where allocQSM     :: InstModsX Allocable       q m cont -> Int ->       cont




newtype NestedFunctor m n a = NestedFunctor { fromNestedFunctor :: m (n a)} deriving (Show)
instance (Functor m, Functor n) => Functor (NestedFunctor m n) where fmap f = NestedFunctor . (fmap $ fmap f) . fromNestedFunctor

nestedLens :: (Functor m, Functor n) => Lens a b c d -> (c -> m (n d)) -> (a -> m (n b))
nestedLens l f = fromNestedFunctor . l (fmap NestedFunctor f)


type MyOperation info q (m :: * -> *) = (ConstraintQuery info q, SuperInst info q m)
--type MyOperation info q (m :: * -> *) w = (ConstraintQuery info q, SuperInst2 info q m w)
type MyOperationCont q info (m :: * -> *) = (ConstraintQuery info q, SuperInst info q m, Functor m, Functor (ResultByQuery info q))


type     OperationCtx' q info m t = (MyOperationCont q (info (DataStoreOf (ContainerOf t))) m, IsContainer t)

type     OperationCtx q info m t = (MyOperationCont q (info (ContainerOf t)) m, IsContainer t)
class    OperationCtx q info m t => Operation2 (q :: [*]) (info :: * -> *) (m :: * -> *) t -- where
instance OperationCtx q info m t => Operation2  q          info             m            t

type BaseOperation = Operation2 '[]
--instance (info ~ ExpandableInfo (ContainerOf t), MyOperationCont q info m, HasContainer2 t) => Operation2 ExpandableInfo q m t



--singletonFF :: MyOperation (AppendableInfo el t) q m => Proxy (q :: [*]) -> el -> t -> m (ResultByQuery (AppendableInfo el t) q t)
--singletonFF = flip runModsF appendQSM


--sizeFF :: (info ~ MeasurableInfo t, MyOperation info q m) => Proxy (q :: [*]) -> t -> m (ResultByQuery info q Int)
--sizeFF = flip runModsF sizeQSM





--singletonFF :: (info ~ SingletonInfo el t, MyOperation info q m) => Proxy (q :: [*]) -> el -> m (ResultByQuery info q t)
--singletonFF = flip runModsF singletonQSM





--appendFF :: MyOperation (AppendableInfo el t) q m => Proxy (q :: [*]) -> el -> t -> m (ResultByQuery (AppendableInfo el t) q t)
--appendFF = flip runModsF appendQSM

--appendCont :: (info ~ AppendableInfo el (ContainerOf t), MyOperationCont q info m, IsContainer t) => Proxy (q :: [*]) -> el -> t -> m (ResultByQuery info q t)
--appendCont q el = fromNestedFunctor . container2 (fmap NestedFunctor $ appendFF q el)



--expandFF :: MyOperation (ExpandableInfo t) q m => Proxy (q :: [*]) -> t -> m (ResultByQuery (ExpandableInfo t) q t)
--expandFF = flip runModsF expandQSM


----expandFF2 :: MyOperation2 (ExpandableInfo t) q m w => Proxy (q :: [*]) -> t -> m (w t)
--expandFF2 :: (Expandable2 cont m q (LstIn (ModsOf Expandable2 cont) q) w) => Proxy q -> cont -> m (w cont)
--expandFF2 = flip runModsF expand2


--expandCont3 :: (Functor m, Functor w, HasContainer2 t, Expandable2 cont m q (LstIn (ModsOf Expandable2 cont) q) w, cont ~ ContainerOf t) => Proxy q -> t -> m (w t)
--expandCont3 q = nestedLens container2 $ expandFF2 q


--singletonCont2 :: (Operation2 q (SingletonInfo el) m t) => Proxy (q :: [*]) -> el -> m (ResultByQuery (SingletonInfo el (ContainerOf t)) q t)
--singletonCont2 q el = (fmap . fmap) fromContainer $ runModsF' singletonQSM q el

--appendCont2 :: (Operation2 q (AppendableInfo el) m t, Simplify (ResultByQuery (AppendableInfo el (ContainerOf t)) q) t a) => Proxy (q :: [*]) -> el -> t -> m a
--appendCont2 q el = fmap simplify . nestedLens container2 (runModsF' appendQSM q el)

--expandCont2 :: (Operation2 q ExpandableInfo m t, Simplify (ResultByQuery (ExpandableInfo (ContainerOf t)) q) t a) => Proxy (q :: [*]) -> t -> m a
--expandCont2 q = fmap simplify . nestedLens container2 (runModsF' expandQSM q)






class                                       Simplify m          t k | m t -> k, k -> t where simplify :: m t -> k
instance {-# OVERLAPPABLE #-}  (k ~ m t) => Simplify m          t k                    where simplify = id
instance                       (k ~   t) => Simplify Simple     t k                    where simplify (Simple a) = a
instance (Simplify m t k', k ~ Maybe k') => Simplify (Maybed m) t k                    where simplify (Maybed ma) = fmap simplify ma



--sizeM :: (MeasurableFinalT q m t, Simplified2 MeasurableInfo q t Int a) => Func' q (t -> m a)
--sizeM = transFunc $ optBuilder $ \q -> fmap simplify . runModsF' sizeQSM q . view container2

--expandM :: (ExpandableFinalT q m t, Simplified ExpandableInfo q t a) => Func' q (t -> m a)
--expandM = transFunc $ optBuilder $ \q -> fmap simplify . nestedLens container2 (runModsF' expandQSM q)

--appendM :: (AppendableFinalT q el m t, Simplified (AppendableInfo el) q t a) => Func' q (el -> t -> m a)
--appendM = transFunc $ optBuilder $ \q el -> fmap simplify . nestedLens container2 (runModsF' appendQSM q el)

--singletonM :: (SingletonFinalT q el m t, Simplified (SingletonInfo el) q t a) => Func' q (el -> m a)
--singletonM = transFunc $ optBuilder $ \q el -> fmap simplify . (fmap . fmap) fromContainer $ runModsF' singletonQSM q el


fmap2 = fmap.fmap
fmap3 = fmap.fmap2
fmap4 = fmap.fmap3
fmap5 = fmap.fmap4




withTransFunc = transFunc .: appFunc

appFunc :: (f -> g) -> OptBuilder opts f -> OptBuilder opts g
appFunc = fmap

--queried

type Func' (q :: [*]) sig = FuncTransBase (Proxy q -> sig) f => f
--type Simplified  info q t     a = Simplified2 info q t t a
--type Simplified2 info q t out a = Simplify (ResultByQuery (info (ContainerOf t)) q) out a



--class AppFunc p f g | p f -> g, g f -> p, g p -> f where
--    appFunc :: p -> f -> g

--instance p ~  (f -> g)              => AppFunc p (OptBuilder opts f) (OptBuilder opts g) where appFunc = fmap
--instance p ~ ((f -> g) -> (h -> i)) => AppFunc p (f -> g)            (h -> i)            where appFunc = ($)


--bar :: SingletonFinal Int m t => m t
--bar :: _ => _
--bar = singletonM (5 :: Int)

--baz :: _ => _
--baz v = queried (Proxy :: Proxy '[Ixed]) expandM v

optBuilder :: a -> OptBuilderBase a
optBuilder = OptBuilder

type OptBuilderBase = OptBuilder '[]
type FuncTransBase  = FuncTrans  '[]
type OperationBase  = Operation2 '[]



--type AppendableFinal    = OperationBase AppendableInfo
--type AppendableFinalT q = Operation2 q  AppendableInfo

type family Ixed' op where Ixed' (Operation2 q info) = Operation2 (Ixed ': q) info


--mytst6 :: (HasContainer2 t, ContOperation q info m, info ~ (RawInfo Expandable (ContainerOf t))) => OptBuilderBase (Proxy (q :: [*]) -> t -> m (ResultByQuery info q t))
--mytst6 = OptBuilder $ flip runModsF expandFX'

--type Func' q info m t = (HasContainer2 t, tinfo ~ info (ContainerOf t), ContOperation q tinfo m, FuncTransBase (Proxy (q :: [*]) -> t -> m (ResultByQuery tinfo q t)) f) => f

--mytst7 :: Func' q ExpandableInfo m t
--mytst7 = transFunc $ mytst6

--mytst7' :: ExpandableFinal t m => t -> m (Simple t)
--mytst7' v = mytst7 v
--class FuncBuilder f a | a -> f where
--    buildFunc :: f -> a

--class FuncTrans opts f a | a opts -> f where
--    transFunc :: OptBuilder opts f -> a


--class    (Monad m, HasContainer2 t, Operation q (RawInfo cls (ContainerOf t)) m) => RawOperation (q :: [*]) (cls :: k) t m -- where


--modFuncF a = transFunc $ buildSchemeF a

--buildSchemeF f = buildFunc (flip runModsF f)

--mytst2 = mytst2

runModsX :: (LstIn (ModsOf inst cont) query ~ matches) => Mods query -> (InstModsX inst query matches cont -> sig) -> sig
runModsX _ f = f InstModsX


--sf a = a

--sft :: _ => _
--sft = modFuncF sf





--type family ResultByQuery (inst :: Constraint)
--type instance ResultByQuery (ExpandableF q m cont) = ResultByQuery_C q cont

--type family ResultByQuery_C (q :: [*]) cont
--type instance ResultByQuery_C '[]              cont = cont
--type instance ResultByQuery_C (Ixed      ': q) cont = ([IndexOf' cont], ResultByQuery_C q cont)
--type instance ResultByQuery_C (Unchecked ': q) cont = ResultByQuery_C q cont






--ResultZ Class.Expandable q t


--class AppendableX    q m cont     el where appendx  :: InstModsX AppendableX      q m cont ->        el -> cont -> MonoResultEl q cont el
--class PrependableX   q m cont     el where prependx :: InstModsX PrependableX     q m cont ->        el -> cont -> MonoResultEl q cont el
--class AddableX       q m cont     el where addx     :: InstModsX AddableX         q m cont ->        el -> cont -> MonoResultEl q cont el
--class RemovableX     q m cont     el where removex  :: InstModsX RemovableX       q m cont ->        el -> cont -> MonoResultEl q cont el

--class Appendable     q m cont     el cont' | q m cont     el -> cont' where appendQSM    :: InstModsX Appendable      q m cont ->        el -> cont -> cont'
--class Prependable    q m cont     el cont' | q m cont     el -> cont' where prependQSM   :: InstModsX Prependable     q m cont ->        el -> cont -> cont'
--class Addable        q m cont     el cont' | q m cont     el -> cont' where addQSM       :: InstModsX Addable         q m cont ->        el -> cont -> cont'
--class Removable      q m cont     el cont' | q m cont     el -> cont' where removeQSM    :: InstModsX Removable       q m cont ->        el -> cont -> cont'

---- === Modification ===
--class Indexable      q m cont idx    el    | q m cont idx    -> el    where index     :: InstModsX Indexable       q m cont -> idx ->       cont -> el
--class Insertable     q m cont idx el cont' | q m cont idx el -> cont' where insert    :: InstModsX Insertable      q m cont -> idx -> el -> cont -> cont'
--class Reservable     q m cont        cont' | q m cont        -> cont' where reserve   :: InstModsX Reservable      q m cont ->              cont -> cont'
--class Releasable     q m cont idx    cont' | q m cont idx    -> cont' where release   :: InstModsX Releasable      q m cont -> idx ->       cont -> cont'

---- === Indexing ===
--class TracksEleQSm    q m cont        els   | q m cont        -> els   where eleQSm     :: InstModsX TracksEleQSm     q m cont ->              cont -> els
--class TracksIxes     q m cont        ixes  | q m cont        -> ixes  where indexes   :: InstModsX TracksIxes      q m cont ->              cont -> ixes
--class TracksFreeIxes q m cont        ixes  | q m cont        -> ixes  where freeIxes  :: InstModsX TracksFreeIxes  q m cont ->              cont -> ixes
--class TracksUsedIxes q m cont        ixes  | q m cont        -> ixes  where usedIxes  :: InstModsX TracksUsedIxes  q m cont ->              cont -> ixes




--Replicable
--head, init, tail, last
--take, drop
--split
--generate
--force?
--update (like in Vector)
--reverse
--elem
--notElem
--find





runMods :: (LstIn (ModsOf inst cont) mods ~ matches) => Mods mods -> (InstMods inst matches -> cont -> out) -> cont -> out
runMods _ f = f InstMods





--moze przez s z typeclass komunikowac kim one sa?

--class b ~ FFoo a => Foo a b where
--    foo :: a -> b

--instance Foo Int String where
--    foo = undefined


----instance Foo t Int where
----    foo = undefined

--type family FFoo a
--type instance FFoo Int = Int
--type instance FFoo String = String

--type instance ModsOf (V.Vector a) Appendable = '[Ixed]

                                                                     --            (cont', tailIdx) = expandIdxed cont

--class (Container cont (IndexOf el cont) el) => Appendable cont     el where appendQSM :: el -> cont -> cont
--                                                                            default appendQSM :: Appendable' cont (IndexOf el cont) el => el -> cont -> cont
--                                                                            appendQSM = fst .: appendQSM'

--class Container cont idx el => Prependable         cont idx el where prependQSM'     :: el -> cont -> (cont, idx)
--class Container cont idx el => Updatable           cont idx el where update       :: idx -> el -> cont -> cont
--class Container cont idx el => Insertable          cont idx el where insert       :: idx -> el -> cont -> cont
--                                                                     unsafeInsert :: idx -> el -> cont -> cont


--class Requestable p where request :: p -> (ElementOf p, p)
--class Releasable  p where release :: ElementOf p -> p -> p

--instance {-# OVERLAPPABLE #-} Indexable cont idx el => Indexable'  cont idx el where unsafeIndex  = index
--instance {-# OVERLAPPABLE #-} Updatable cont idx el => Insertable' cont idx el where unsafeInsert = update

-- utils

--growQSM :: Growable cont => Int -> cont -> cont
--growQSM i cont = if i < 0 then error "negative index"
--                       else unsafeGrow i cont

--appendQSM :: Appendable' cont idx el => el -> cont -> cont
--appendQSM = fst .: appendQSM'

--prependQSM :: Prependable cont idx el => el -> cont -> cont
--prependQSM = fst .: prependQSM'

--allocQSM i = if i < 0 then error $ "negative length " <> show i
--                   else unsafeAlloc i

-- nie powinno byc czegos takiego jak unsafe (metody safe zwracaja maybe) - poniewaz mozna zrobic jedna generalna metode ktora sprawdza czy index jest w zakresach i zwraca maybe (!)


--data Unchecked a = Unchecked a deriving (Show, Functor)

--unsafe       = Unchecked
--unsafely f a = f $ Unchecked a

--instance Wrap    Unchecked where wrap              = Unchecked
--instance Unwrap  Unchecked where unwrap (Unchecked a) = a
--instance Wrapped Unchecked



--instance HasContainer (Unchecked a) (Unchecked a) where container = id


--type instance ElementOf       (Unchecked a) = ElementOf a
--type instance ElementByIx idx (Unchecked a) = ElementByIx idx a
--type instance IndexOf     el  (Unchecked a) = IndexOf el a


--instance Measurable a       => Measurable (Unchecked a) where sizeQSM = sizeQSM . unwrap
--instance Container a idx el => Container (Unchecked a) idx el where eleQSm   = eleQSm   . unwrap
--                                                                 indexes = indexes . unwrap

--index2' = index2 emptyOpts


--instance
--instance {-# OVERLAPPABLE #-} (t ~ (f -> g), opts ~ '[]) => FuncBuilder (OptBuilder opts f) (a -> b) where buildFunc = undefined

--instance (t ~ (Mods opts -> a -> b)) => FuncBuilder (OptBuilder opts t) (Mods opts -> a -> b) where buildFunc (OptBuilder f) = f Mods


extendOptBuilder :: Proxy opt -> OptBuilder opts a -> OptBuilder (opt ': opts) a
extendOptBuilder _ (OptBuilder a) = OptBuilder a

setOptBuilder :: Proxy opts -> OptBuilder old a -> OptBuilder opts a
setOptBuilder _ (OptBuilder a) = OptBuilder a

type ModConstraint mod  = FuncTrans (mod ': opts) f g => OptBuilder opts f -> g
type SetConstraint opts = FuncTrans opts f g => OptBuilder old f -> g


modConstraint :: Proxy mod -> ModConstraint mod
modConstraint = transFunc .: extendOptBuilder

setConstraint :: Proxy opts -> SetConstraint opts
setConstraint = transFunc .: setOptBuilder

unchecked :: ModConstraint Unchecked
unchecked = modConstraint (Proxy :: Proxy Unchecked)

--uncheckedIf :: ModConstraint Unchecked
--uncheckedIf cond v = ifT cond (unchecked v) 'l'

safe :: ModConstraint Safe
safe = modConstraint (Proxy :: Proxy Safe)

ixed :: ModConstraint Ixed
ixed = modConstraint (Proxy :: Proxy Ixed)

try :: ModConstraint Try
try = modConstraint (Proxy :: Proxy Try)

queried :: Proxy opts -> SetConstraint opts
queried = setConstraint


--ixed' = transFunc $ optBuilder ixed
--class    IfT (cond :: Bool) where ifT :: Proxy cond -> a -> b -> If cond a b
--instance IfT True           where ifT _ = const
--instance IfT False          where ifT _ = flip const

class    Cond (cond :: Bool) where ifT :: Proxy cond -> a -> a -> a
instance Cond True           where ifT _ = const
instance Cond False          where ifT _ = flip const



class UncheckedIf (cond :: Bool) opts opts' | cond opts -> opts', cond opts' -> opts where uncheckedIf :: Proxy cond -> OptBuilder opts f -> OptBuilder opts' f
instance UncheckedIf True  opts (Unchecked ': opts) where uncheckedIf _ (OptBuilder f) = OptBuilder f
instance UncheckedIf False opts opts                where uncheckedIf _ = id

uncheckedIf' :: (FuncTrans opts f y, UncheckedIf cond opts1 opts) => Proxy (cond :: Bool) -> OptBuilder opts1 f -> y
uncheckedIf' = transFunc .: uncheckedIf












class Result2 (i :: Bool) t f g | i t f -> g, i g -> t  where
    result2 :: a ~ ResultElem i f => Proxy i -> Lens (t a') (t a) a' a -> (a' -> f) -> (t a' -> g)

type family ResultElem i a where
    ResultElem True  (f a) = a
    ResultElem False a     = a

instance {-# OVERLAPPABLE #-} Result2 False t a     (t a)     where result2 _ lens f = lens %~ f
instance Functor f         => Result2 True  t (f a) (f (t a)) where result2 _ lens f = lens f

checkQuery :: Proxy q -> Proxy (In Ixed q)
checkQuery _ = Proxy



type Result2' q = Result2 (In Ixed q)

result2' :: (a ~ ResultElem i f, i ~ In Ixed q, Result2 i t f g) => Proxy q -> Lens (t a') (t a) a' a -> (a' -> f) -> (t a' -> g)
result2' = result2 . checkQuery



wrappedResult :: (a ~ ResultElem i f, i ~ In Ixed q, Result2 i t f g, Wrapped t) => Proxy q -> (a' -> f) -> (t a' -> g)
wrappedResult q = result2' q wrapped


runWrapped0 s f = wrappedResult (query s) $   f (polySpecX s)
runWrapped1 s f = wrappedResult (query s) .   f (polySpecX s)
runWrapped2 s f = wrappedResult (query s) .:  f (polySpecX s)
runWrapped3 s f = wrappedResult (query s) .:. f (polySpecX s)
runWrapped4 s f = wrappedResult (query s) .:: f (polySpecX s)


--type InstFunc2 (inst :: [Bool] -> * -> k) mods cont sig = InstMods2 inst (LstIn (ModsOf inst cont) mods) cont -> sig
--type SchemeBuilder2 cont mods sig scheme = FuncBuilder (Mods mods -> sig) scheme => InstFunc2 inst mods cont sig -> scheme



--buildScheme2 :: SchemeBuilder2 inst cont mods out scheme



--modFunc2 = transFunc . buildScheme2




--type InstFunc2 (inst :: [Bool] -> * -> k) mods cont sig = InstMods2 inst (LstIn (ModsOf inst cont) mods) cont -> sig

--type InstFunc2 inst mods cont sig = InstMods2 inst (LstIn (ModsOf inst cont) mods) cont -> sig

--buildScheme2 :: (FuncBuilder (Mods mods -> sig) a) => (InstMods2 inst (LstIn (ModsOf inst cont) mods) cont -> sig) -> a
--buildScheme2 :: SchemeBuilder2 cont mods sig scheme

