{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Containers.Poly where

import Prologue hiding (Ixed, Indexed, Simple)
import           Data.TypeLevel.List (In)
import Data.TypeLevel.Bool
import Data.Typeable
import GHC.Prim
import Data.TypeLevel.Bool





------------------

type family CmpLst (lst :: [k]) (lst' :: [k']) :: [Bool] where
            CmpLst (l ': ls)    (l  ': ls')     = (True  ': CmpLst ls ls')
            CmpLst (l ': ls)    (l' ': ls')     = (False ': CmpLst ls ls')
            CmpLst '[]          '[]             = '[]

type family LstIn (lst :: [k]) (lst' :: [k']) :: [Bool] where
            LstIn (l ': ls) lst = In l lst ': LstIn ls (Remove l lst)
            LstIn ls        '[] = '[]
            LstIn '[]       lst = '[]


type family   Remove (a :: k) (cont :: c) :: c
type instance Remove (a :: k) ((t ': ts) :: [k]) = If (a :== t) ts (t ': Remove a ts)
type instance Remove (a :: k) '[]                = '[]


type family ModsOf (inst :: k) cont :: [*]
data Mods (opts :: [*]) = Mods
--data Mods (mods :: [Bool]) = Mods
data InstMods (inst :: k) (mods :: [Bool]) = InstMods
data InstMods2 (inst :: k) (mods :: [Bool]) (cont :: *) = InstMods2
data InstModsX (inst :: k) (query :: [*]) (mods :: [Bool]) (cont :: *) = InstModsX

type I = InstMods2
type I2X = InstModsX

type family UpdateQuery instMods guery where
    UpdateQuery (InstModsX inst q m cont) q'= InstModsX inst q' (LstIn (ModsOf inst cont) q') cont



type family Foxo inst query where
    Foxo (inst query mods cont) query' = inst query' (LstIn (ModsOf inst cont) query') cont

type family Lolo a inst where
    Lolo a ((inst :: [*] -> [Bool] -> * -> k) query mods cont) = (inst (a ': query) (LstIn (ModsOf inst cont) (a ': query)) cont)

type family Lolo2 a inst where
    Lolo2 a ((inst :: [*] -> [Bool] -> * -> k) query mods cont) = (inst (a ': query) (LstIn (ModsOf inst cont) (a ': query)) cont)

    --Lolo ((inst :: [*] -> [Bool] -> * -> k) query mods cont) a = inst query mods cont
    --Lolo ((inst :: [*] -> [Bool] -> * -> k) query mods cont) a = inst (a ': query) (LstIn (ModsOf inst cont) (a ': query)) cont


polyspec :: InstMods2 inst mods cont -> InstMods2 inst mods cont'
polyspec _ = InstMods2

rebaseSpec :: InstMods2 inst mods cont -> InstMods2 inst' mods cont
rebaseSpec _ = InstMods2


rebaseSpecX :: InstModsX inst q mods cont -> InstModsX inst' q (LstIn (ModsOf inst' cont) q) cont
rebaseSpecX _ = InstModsX

polySpecX :: InstModsX inst q mods cont -> InstModsX inst q (LstIn (ModsOf inst cont') q) cont'
polySpecX _ = InstModsX
--type family Rebase mods base where Rebase (InstMods2 inst mods old) new = InstMods2 inst mods new

type family InstQuery (inst :: * -> Constraint) :: [*]
type family InstFunc' (inst :: * -> Constraint) :: *

query :: InstModsX inst q mods cont -> Proxy q
query _ = Proxy

type family Falses (lst :: [k]) :: [Bool] where
    Falses (a ': as) = False ': Falses as
    Falses '[]       = '[]



--class Expandable     q m cont        cont' | q m cont        -> cont' where expand    :: InstModsX Expandable      q m cont ->              cont -> cont'
--class ExpandableF'               cont m q s where expandF'   :: Monad m => Query q s -> ExpandableInfo' cont ->              cont -> m (ResultByQuery (Selected s (ModsOf cont ExpandableF')) (ExpandableInfo' cont) cont)



type ProxyInst inst = Proxy (inst :: [*] -> [Bool] -> * -> k)
-- FIXME[wd]: Inst' nie dziala - np. w Resizable.hs
type Inst'     (inst :: [Bool] -> * -> k)      cont     = Inst inst ('[] :: [Bool]) cont
type Inst      (inst :: [Bool] -> * -> k) mods cont     =          inst (LstIn (ModsOf inst cont) mods)    cont
type InstFunc  (inst :: [Bool] -> * -> k) mods cont out = InstMods inst (LstIn (ModsOf inst cont) mods) -> cont -> out
type InstFunc2 (inst :: [Bool] -> * -> k) mods cont sig = InstMods2 inst (LstIn (ModsOf inst cont) mods) cont -> sig

type InstX     (inst :: [*] -> [Bool] -> * -> k) mods cont     = inst mods (LstIn (ModsOf inst cont) mods)    cont
type InstFuncX (inst :: [*] -> [Bool] -> * -> k) mods cont sig = InstModsX inst mods (LstIn (ModsOf inst cont) mods) cont -> sig

type InstX3      (inst :: [Bool] -> * -> k) mods cont     =          inst (LstIn (ModsOf inst cont) mods)    cont


type InstX2    (inst :: [*] -> [Bool] -> * -> k) mods     = inst mods '[]



type SchemeBuilder2 cont mods sig scheme = forall inst. FuncBuilder (Mods mods -> sig) scheme => InstFunc2 inst mods cont sig -> scheme


type DOO mods c a inst cont = (FuncBuilder (Mods mods -> sig) a) => (InstMods2 inst (LstIn (ModsOf inst cont) mods) cont -> sig) -> a

type DOOX query c a inst cont = (FuncBuilder (Mods query -> sig) a) => (InstModsX inst query (LstIn (ModsOf inst cont) query) cont -> sig) -> a

buildScheme2 :: DOO mods c a inst cont
buildScheme2 f = buildFunc (flip runMods2 f)

buildSchemeX :: DOOX query c a inst cont
buildSchemeX f = buildFunc (flip runModsX f)

--modFuncX :: (FuncTrans '[] (Mods query -> sig) b) => (InstModsX inst query (LstIn (ModsOf inst cont) query) cont -> sig) -> b
modFuncX :: InstFuncX inst mods cont sig -> Func mods sig
modFuncX = transFunc . buildSchemeX

buildSchemeF f = buildFunc (flip runModsF f)
modFuncF a = transFunc $ buildSchemeF a

modFunc2 :: InstFunc2 inst mods cont sig -> Func mods sig
modFunc2 = transFunc . buildScheme2

--modFunc2' :: _ => _
--modFunc2' = transFunc . buildScheme2


runMods2 :: (LstIn (ModsOf inst cont) mods ~ matches) => Mods mods -> (InstMods2 inst matches cont -> sig) -> sig
runMods2 _ f = f InstMods2

runModsX :: (LstIn (ModsOf inst cont) query ~ matches) => Mods query -> (InstModsX inst query matches cont -> sig) -> sig
runModsX _ f = f InstModsX

type Scheme               sig = FuncBuilder                 sig  f => f
type ModFunc     sel mods sig = FuncTrans sel (Mods mods -> sig) f => f
type Func            mods sig = ModFunc   '[]       mods    sig



type SchemeBuilder inst cont mods out scheme = FuncBuilder (Mods mods -> cont -> out) scheme => InstFunc inst mods cont out -> scheme


buildScheme :: SchemeBuilder inst cont mods out scheme
buildScheme f = undefined

modFunc :: InstFunc inst mods cont sig -> Func mods (cont -> sig)
modFunc = transFunc . buildScheme



emptyOpts = Mods :: Mods '[]



type EmptyPolyLst = ('[] :: [k])




newtype OptBuilder (opts :: [*]) a = OptBuilder a deriving (Functor)

class FuncBuilder f a | a -> f where
    buildFunc :: f -> a

class FuncTrans opts f a | a opts -> f where
    transFunc :: OptBuilder opts f -> a

instance {-# OVERLAPPABLE #-} (f ~ a, g ~ b)             => FuncBuilder (f -> g)            (a -> b)            where buildFunc = id
instance {-# OVERLAPPABLE #-} (t ~ (f -> g), opts ~ '[]) => FuncBuilder (f -> g)            (OptBuilder opts t) where buildFunc = OptBuilder

instance                            (opts ~ opts', f ~ f')       => FuncTrans opts f  (OptBuilder opts' f') where transFunc = id
instance                          (f ~ (Proxy opts -> a -> b))   => FuncTrans opts f  (a -> b)              where transFunc (OptBuilder f) = f Proxy




type EmptyStarLst = ('[] :: [*])



--type family Foo (lst (a :: k) where
--    Foo


--type ResultByQuery' (s :: [Bool]) (cont :: *) (info :: *) (cls :: k) = ResultByQuery (Selected s (ModsOf cls cont)) info cont




--------------





type family Selected (b :: [Bool]) (lst :: [k]) :: [k] where
    Selected ('True  ': b) (l ': ls) = l ': Selected b ls
    Selected ('False ': b) (l ': ls) =      Selected b ls
    Selected '[]           lst       =      '[]
    Selected s             lst       =      '[]



data NA = NA
data Info (idx :: *) (el :: *) (cls :: k) (cont :: *) = Info
data Query (q :: [*]) (m :: [Bool]) = Query

freeQuery :: Query q s -> Query q s'
freeQuery _ = Query

freeInfo :: Info idx el cls cont -> Info idx el cls cont'
freeInfo _ = Info

type family InfoIdx  i where InfoIdx  (Info idx el cls cont) = idx
type family InfoEl   i where InfoEl   (Info idx el cls cont) = el
type family InfoCls  i where InfoCls  (Info idx el cls cont) = cls
type family InfoCont i where InfoCont (Info idx el cls cont) = cont


type family Operation (q :: [*]) info where Operation q (Info idx el cls cont) = Delayed q (LstIn (ModsOf cls cont) q) (Info idx el cls cont)



class Monad m => Delayed  q s info m where delayed  :: Query q s -> info -> InfoCont info -> m (ResultByQuery info q (InfoCont info))
class Monad m => Delayed2 q s info m where delayed2 :: Query q s -> info                  -> m (ResultByQuery info q (InfoCont info))


--class Monad m => GenOp    q s info m out | info -> out where genOp    :: Query q s -> info ->                InfoCont info -> m (ResultByQuery info q (InfoCont info))
--class Monad m => ContOp   q s info m out | info -> out where contOp   :: Query q s -> info ->                InfoCont info -> m (ResultByQuery info q out)
--class Monad m => ElContOp q s info m out | info -> out where elcontOp :: Query q s -> info -> InfoEl info -> InfoCont info -> m (ResultByQuery info q out)


--class Monad m => FX2  q s info m where fx2  :: Query q s -> info -> (InfoCont info) -> m (ResultByQuery q info (InfoCont info))


type CheckSelection q s cont info mods = ResultByQuery info q ~ ResultByQuery info (Selected s mods)

type RawInfo           = Info NA  NA
type IxedInfo   idx    = Info idx NA
type ElInfo         el = Info NA  el
type IxedElInfo idx el = Info idx el



type family ElementOf        cont
type family IndexOf      el  cont
type family ElementByIx idx cont
type family IxType      idx

type IndexOf' cont = IndexOf (ElementOf cont) cont

-- === Results ===

newtype Simple  a = Simple a deriving (Show, Functor)
type Indexed i = (,) i

simple :: Monad m => a -> m (Simple a)
simple = return . Simple

type family PrependTuple i (a :: * -> *) :: * -> *
type instance PrependTuple i Simple = (,) i
type instance PrependTuple i ((,) t1) = (,,) i t1
type instance PrependTuple i ((,,) t1 t2) = (,,,) i t1 t2
type instance PrependTuple i ((,,,) t1 t2 t3) = (,,,,) i t1 t2 t3
type instance PrependTuple i ((,,,,) t1 t2 t3 t4) = (,,,,,) i t1 t2 t3 t4
type instance PrependTuple i ((,,,,,) t1 t2 t3 t4 t5) = (,,,,,,) i t1 t2 t3 t4 t5


type family   ResultByQuery info (q :: [*]) :: * -> *
type instance ResultByQuery (Info idx el cls cont) '[]              = Simple
type instance ResultByQuery (Info idx el cls cont) (Unchecked ': q) = ResultByQuery (Info idx el cls cont) q
type instance ResultByQuery (Info idx el cls cont) (Ixed      ': q) = If (idx :== NA)
                                                                         (PrependResultTupleidx q cls cont (IxedData cls (IndexOf' cont))   el)
                                                                          --If (el :== NA)
                                                                          --    (PrependResultTupleidx q cls cont (IndexOf' cont)   el)
                                                                          --    (PrependResultTupleidx q cls cont (IndexOf el cont) el))
                                                                         (PrependResultTupleidx q cls cont (IxedData cls idx) el)
type instance ResultByQuery (Info idx el cls cont) (Try       ': q) = Maybed (ResultByQuery (Info idx el cls cont) q)

newtype Maybed a t = Maybed (Maybe (a t)) deriving (Show, Functor)

type IxedData cls idx = If (IxedMode cls :== Single) idx [idx]

type PrependResultTupleidx q cls cont idx el = (PrependTuple idx (ResultByQuery (Info idx el cls cont) q))


type family ResultByQuery' (info :: *) (s :: [Bool]) where ResultByQuery' (Info idx el cls cont) s = ResultByQuery (Info idx el cls cont) (Selected s (ModsOf cls cont)) cont

type family ResultBySel (info :: *) (s :: [Bool]) where ResultBySel (Info idx el cls cont) s = ResultByQuery (Info idx el cls cont) (Selected s (FilterMutable (ModsOf cls cont)))



type family FilterMutable (lst :: [*]) :: [*] where
    FilterMutable '[] = '[]
    FilterMutable (l ': ls) = If (Mutable l) (l ': FilterMutable ls) (FilterMutable ls)

type family ResultMod q base info
type instance ResultMod Unchecked b i = b

type instance ResultMod Ixed      b (Info idx el cls cont) = b

type ResultCheck     q info r   = ResultByQuery  info q ~ r
type SimpleResult    q info     = ResultCheck  q info Simple
type IxedResult      q info i   = ResultCheck  q info (Indexed i)
type SimpleRawResult cls q cont = SimpleResult q (RawInfo cls cont)



type ComputeSelection (cls :: k) (cont :: *) (q :: [*]) = LstIn (ModsOf cls cont) q

type CheckQuery i q s = ResultByQuery i q ~ ResultBySel i s

type MatchResults i s i' q' = (ResultBySel i s ~ ResultByQuery i' q')

type SubOperation i s i' q' m = (MatchResults i s i' q', ContOperation q' i' m)

type ContOperation q info m = (Monad m, Operation q info m, Functor (ResultByQuery info q))

type MatchResultsCls i t t' s q = MatchResults (i t) s (i (ContainerOf t')) q



runModsF :: LstIn (ModsOf cls cont) q ~ s => (Proxy (q :: [*])) -> (Query q s -> Info idx el cls cont -> sig) -> sig
runModsF _ f = f Query Info


runModsF' = flip runModsF


type family InsertQuery a op where
    InsertQuery a (RawOperation q cls) = RawOperation (a ': q) cls


class    (Monad m, HasContainer2 t, Operation q (RawInfo cls (ContainerOf t)) m) => RawOperation (q :: [*]) (cls :: k) t m -- where
instance (Monad m, HasContainer2 t, Operation q (RawInfo cls (ContainerOf t)) m) => RawOperation  q          cls       t m



type family ContainerOf a
class HasContainer2 a where
    container2 :: Lens' a (ContainerOf a)

--data Info idx el (cls :: k) cont = Info

type ConstraintQuery info q = CheckQuery info q (InfoSelection info q)

type family InfoSelection i q where InfoSelection (Info idx el cls cont) q = ComputeSelection cls cont q


-- superinst powinien instancjonowac na podstawie N/A !
type family SuperInst info q m where SuperInst (Info NA  NA cls cont) q m = cls        cont m q (ComputeSelection cls cont q)
                                     SuperInst (Info NA  el cls cont) q m = cls     el cont m q (ComputeSelection cls cont q)
                                     SuperInst (Info idx NA cls cont) q m = cls idx    cont m q (ComputeSelection cls cont q)
                                     SuperInst (Info idx el cls cont) q m = cls idx el cont m q (ComputeSelection cls cont q)

type family SuperInst2 info q m w where SuperInst2 (Info NA  NA cls cont) q m w = cls        cont m q (ComputeSelection cls cont q) w
                                        SuperInst2 (Info NA  el cls cont) q m w = cls     el cont m q (ComputeSelection cls cont q) w
                                        SuperInst2 (Info idx NA cls cont) q m w = cls idx    cont m q (ComputeSelection cls cont q) w
                                        SuperInst2 (Info idx el cls cont) q m w = cls idx el cont m q (ComputeSelection cls cont q) w



----



data Safe      = Safe
data Unchecked = Unchecked
data Ixed      = Ixed
data Try       = Try


type family Mutable a :: Bool
type instance Mutable Ixed      = True
type instance Mutable Unchecked = False
type instance Mutable Try       = True



type family IxedX (op :: k) :: l where IxedX (cls q (m ::  * -> *) :: * -> Constraint) = cls (Ixed ': q) m
                                       IxedX (cls q    :: (* -> *) -> * -> Constraint) = cls (Ixed ': q)




-- === Concatenation ===

type family   MonoResultEl (q :: [*])       cont el
--type instance MonoResultEl '[]              cont el = cont
--type instance MonoResultEl (Ixed      ': q) cont el = (IndexOf el cont, MonoResultEl q cont el)
--type instance MonoResultEl (Unchecked ': q) cont el = MonoResultEl q cont el

type family   MonoResult (q :: [*])       cont
--type instance MonoResult '[]              cont = cont
--type instance MonoResult (Ixed      ': q) cont = (IndexOf' cont, MonoResult q cont)
--type instance MonoResult (Unchecked ': q) cont = MonoResult q cont

type family   PolyResultEl (q :: [*])       cont el
--type instance PolyResultEl '[]              cont el = cont
--type instance PolyResultEl (Ixed      ': q) cont el = ([IndexOf el cont], PolyResultEl q cont el)
--type instance PolyResultEl (Unchecked ': q) cont el = PolyResultEl q cont el

type family   ResultZ (inst :: [*] -> [Bool] -> * -> k) (q :: [*]) cont
--type instance ResultZ inst '[]              cont = cont
--type instance ResultZ inst (Ixed      ': q) cont = ([IndexOf' cont], ResultZ inst q cont)
--type instance ResultZ inst (Unchecked ': q) cont = ResultZ inst q cont

type family   ResultZ2 (inst :: [*] -> [Bool] -> * -> k) (q :: [*]) t t'
--type instance ResultZ2 inst '[]              t t' = t'
--type instance ResultZ2 inst (Ixed      ': q) t t' = ([IndexOf' t], ResultZ2 inst q t t')
--type instance ResultZ2 inst (Unchecked ': q) t t' = ResultZ2 inst q t t'








type family IxedMode (a :: k) :: IxedType

data IxedType = Multi
              | Single
              deriving (Show)