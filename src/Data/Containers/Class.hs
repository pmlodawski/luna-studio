{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE RankNTypes      #-}

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

type IsContainer a = HasContainer a a


--- === Containers ===



--class Measurable a where size :: Integral i => a -> i

-- GHC BUG [WD]: we can remove fundeps here when it will be fixed: https://ghc.haskell.org/trac/ghc/ticket/10778
class (IndexOf el cont ~ idx, ElementByIx idx cont ~ el) => Container cont idx el | el cont -> idx, idx cont -> el where
    --elems   :: cont -> [el]
    --indexes :: cont -> [idx]


-- === Finite ===
class Measurable     q m cont        size  | q m cont        -> size  where size      :: InstModsX Measurable      q m cont ->              cont -> size
class MinIndexed     q m cont        idx   | q m cont        -> idx   where minIndex  :: InstModsX MinIndexed      q m cont ->              cont -> idx
class MaxIndexed     q m cont        idx   | q m cont        -> idx   where maxIndex  :: InstModsX MaxIndexed      q m cont ->              cont -> idx

-- === Construction ===
class Singleton      q m cont     el                                  where singleton :: InstModsX Singleton       q m cont ->        el -> cont
class Allocable      q m cont                                         where alloc     :: InstModsX Allocable       q m cont -> Int ->       cont
class Growable       q m cont        cont' | q m cont        -> cont' where grow      :: InstModsX Growable        q m cont -> Int ->       cont -> cont'
class Expandable     q m cont        cont' | q m cont        -> cont' where expand    :: InstModsX Expandable      q m cont ->              cont -> cont'


--class ExpandableF    q s cont m        where expandF    :: Monad m => Query q s -> ExpandableInfo cont ->               cont -> m (ResultByQuery q (ExpandableInfo cont) cont)

--type ResultByQuery' = ResultByQuery (Selected s (ModsOf cont ExpandableF')) (ExpandableInfo cont) cont
--type ResultByQuery' info s = ResultByQuery (Selected s (ModsOf (InfoCont info) (InfoCls info))) (ExpandableInfo (InfoCont info)) (InfoCont info)
--type family ResultByQuery' info s :: * -> * where ResultByQuery' (Info idx el cls cont) s = ResultByQuery (Selected s (ModsOf cont cls)) (cls cont)

--class AppendableX    q m cont     el where appendx  :: InstModsX AppendableX      q m cont ->        el -> cont -> MonoResultEl q cont el
--class Singleton      q m cont     el                                  where singleton :: InstModsX Singleton       q m cont ->        el -> cont


type ExpandableInfo    = RawInfo ExpandableF'
type AppendableInfo el = ElInfo el AppendableF'


class ExpandableF'               cont m q s where expandF'    :: info ~ RawInfo    ExpandableF' cont => Query q s -> info ->       cont -> m (ResultBySel info s cont)
class AppendableF'            el cont m q s where appendF'    :: info ~ ElInfo  el AppendableF' cont => Query q s -> info -> el -> cont -> m (ResultBySel info s cont)
class SingletonF'             el cont m q s where singletonF' :: info ~ ElInfo  el SingletonF'  cont => Query q s -> info -> el         -> m (ResultBySel info s cont)

--class ExpandableF'               cont m q s where expandF'   :: (Monad m, info ~ (RawInfo ExpandableF' cont)) => Query q s -> ExpandableInfo cont -> cont -> m (ResultBySel info s cont)

--class ExpandableF'               cont m q s where expandF'   :: Monad m => Query q s -> ExpandableInfo cont ->              cont -> m (ResultByQuery' (ExpandableInfo cont) s cont)


--foo :: (cls ~ ExpandableF', Monad m, info ~ ExpandableInfo cont, CheckQuery info q s, cls cont m q s) => Query q s -> info -> InfoCont info -> m (ResultByQuery info q (InfoCont info))
--foo = expandF'

--class Monad m => Bar q s cont info m where bar :: Query q s -> info ->              cont -> m (ResultByQuery (Selected s (ModsOf cont (InfoCls info))) info cont)


--class Monad m => Delayed  q s info m where fx  :: Query q s -> info -> (InfoCont info) -> m (ResultByQuery info q (InfoCont info))
instance (Monad m, cls ~ ExpandableF', CheckQuery (ExpandableInfo cont) q s, cls cont m q s) => Delayed (q :: [*]) s (ExpandableInfo cont) m where delayed = expandF'
--instance (Monad m, cls ~ ExpandableF', CheckQuery (ExpandableInfo cont) q s, cls cont m q s) => Delayed (q :: [*]) s (ExpandableInfo cont) m where delayed = expandF'

--type CheckQuery q s info cont cls = ResultByQuery i q ~ ResultBySel i s

--class Monad m => ContOp   q s info m out | info -> out where contOp  :: Query q s -> info ->                InfoCont info -> m (ResultByQuery info q out)

--instance (Monad m, cls ~ ExpandableF', CheckQuery (ExpandableInfo    cont) q s, cls    cont m q s) => ContOp   (q :: [*]) s (ExpandableInfo    cont) m cont where contOp   = expandF'
--instance (Monad m, cls ~ AppendableF', CheckQuery (AppendableInfo el cont) q s, cls el cont m q s) => ElContOp (q :: [*]) s (AppendableInfo el cont) m cont where elcontOp = appendF'
--instance (Monad m, cls ~ ExpandableF', CheckQuery (ExpandableInfo cont) q s, cls cont m q s) => ContOp (q :: [*]) s (ExpandableInfo cont) m cont where contOp = expandF'


--class Monad m => ElContOp q s info m out | info -> out where elcontOp :: Query q s -> info -> InfoEl info -> InfoCont info -> m (ResultByQuery info q out)




type ExpandableFX q s cont m = Delayed q s (ExpandableInfo cont) m



newtype NestedFunctor m n a = NestedFunctor { fromNestedFunctor :: m (n a)} deriving (Show)
instance (Functor m, Functor n) => Functor (NestedFunctor m n) where fmap f = NestedFunctor . (fmap $ fmap f) . fromNestedFunctor

nestedLens :: (Functor m, Functor n) => Lens a b c d -> (c -> m (n d)) -> (a -> m (n b))
nestedLens l f = fromNestedFunctor . l (fmap NestedFunctor f)


expandFX :: (info ~ (ExpandableInfo cont), ResultByQuery (ExpandableInfo cont) q ~ ResultByQuery (Info NA NA ExpandableF' cont) (Selected s (ModsOf ExpandableF' cont)), ExpandableF' cont m q s) => Query q s -> info ->               cont -> m (ResultByQuery info q cont)
expandFX = expandF'

--expandFX' :: (Delayed q s info m) info ~ (ExpandableInfo cont)) => Query q s -> info ->               cont -> m (ResultByQuery info q cont)
--expandFX' = fx

expandFX' :: (Delayed q s info m, info ~ (ExpandableInfo (ContainerOf t)), HasContainer2 t, Functor (ResultByQuery info q)) => Query q s -> info ->               t -> m (ResultByQuery info q t)
expandFX' q i = fromNestedFunctor . container2 (fmap NestedFunctor $ delayed q i)

--monadLens :: Monad f => Lens (f a) (f a') a a'
--monadLens = lens undefined (const return) -- (\a x -> fmap (const a) x)

--expandFX' :: (Delayed q s info m, info ~ (ExpandableInfo cont)) => Query q s -> info ->               cont -> m (ResultByQuery info q cont)
--expandFX' q i c = do where
--    f <- expandFX q i (view container c)

--type ExpandableInfo cont = RawInfo ExpandableF cont

class (Monad m, Operation q info m) => Operation2 (q :: [*]) info m -- where
instance (Monad m, Operation q info m) => Operation2 q info m


type Ixed' op = InsertQuery Ixed op

type ExpandableFinalT (q :: [*]) = RawOperation q ExpandableF'
type ExpandableFinal             = ExpandableFinalT '[]

--mytst :: Operation2 '[] (ExpandableInfo cont) m => cont -> m (Simple cont)
--mytst = runModsF (Proxy :: Proxy '[]) expandFX'
mytst :: ExpandableFinal t m => t -> m (Simple t)
mytst = transFunc $ buildFunc $ flip runModsF expandFX'

--mytst2 :: ExpandableFinalT '[Ixed] cont m => cont -> m (NA, cont)
mytst2 :: Ixed' ExpandableFinal t m => t -> m (IndexOf' (ContainerOf t), t)
mytst2 = ixed $ transFunc $ buildSchemeF expandFX'

mytst3 :: ExpandableFinal t m => t -> m (Simple t)
mytst3 = runModsF (Proxy :: Proxy '[]) expandFX'


mytst4 :: (HasContainer2 t, ContOperation q info m, info ~ (RawInfo ExpandableF' (ContainerOf t))) => Proxy (q :: [*]) -> t -> m (ResultByQuery info q t)
mytst4 = flip runModsF expandFX'

mytst4' :: (ContOperation q info m, info ~ (RawInfo ExpandableF' t), ResultByQuery (Info NA NA ExpandableF' t) (Selected (LstIn (ModsOf ExpandableF' t) q) (ModsOf ExpandableF' t)) ~ ResultByQuery (RawInfo ExpandableF' t) q, ExpandableF' t m q (LstIn (ModsOf ExpandableF' t) q)) => Proxy (q :: [*]) -> t -> m (ResultByQuery info q t)
mytst4' = flip runModsF expandFX

mytst5 :: (HasContainer2 t, ContOperation q info m, info ~ (RawInfo ExpandableF' (ContainerOf t)), FuncBuilder (Proxy (q :: [*]) -> t -> m (ResultByQuery info q t)) f) => f
mytst5 = buildFunc $ flip runModsF expandFX'


type OptBuilderBase = OptBuilder '[]
type FuncTransBase  = FuncTrans  '[]

mytst6 :: (HasContainer2 t, ContOperation q info m, info ~ (RawInfo ExpandableF' (ContainerOf t))) => OptBuilderBase (Proxy (q :: [*]) -> t -> m (ResultByQuery info q t))
mytst6 = OptBuilder $ flip runModsF expandFX'

type Func' q info m t = (HasContainer2 t, tinfo ~ info (ContainerOf t), ContOperation q tinfo m, FuncTransBase (Proxy (q :: [*]) -> t -> m (ResultByQuery tinfo q t)) f) => f

mytst7 :: Func' q ExpandableInfo m t
mytst7 = transFunc $ mytst6

mytst7' :: ExpandableFinal t m => t -> m (Simple t)
mytst7' v = mytst7 v
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


class AppendableX    q m cont     el where appendx  :: InstModsX AppendableX      q m cont ->        el -> cont -> MonoResultEl q cont el
class PrependableX   q m cont     el where prependx :: InstModsX PrependableX     q m cont ->        el -> cont -> MonoResultEl q cont el
class AddableX       q m cont     el where addx     :: InstModsX AddableX         q m cont ->        el -> cont -> MonoResultEl q cont el
class RemovableX     q m cont     el where removex  :: InstModsX RemovableX       q m cont ->        el -> cont -> MonoResultEl q cont el

class Appendable     q m cont     el cont' | q m cont     el -> cont' where append    :: InstModsX Appendable      q m cont ->        el -> cont -> cont'
class Prependable    q m cont     el cont' | q m cont     el -> cont' where prepend   :: InstModsX Prependable     q m cont ->        el -> cont -> cont'
class Addable        q m cont     el cont' | q m cont     el -> cont' where add       :: InstModsX Addable         q m cont ->        el -> cont -> cont'
class Removable      q m cont     el cont' | q m cont     el -> cont' where remove    :: InstModsX Removable       q m cont ->        el -> cont -> cont'

-- === Modification ===
class Indexable      q m cont idx    el    | q m cont idx    -> el    where index     :: InstModsX Indexable       q m cont -> idx ->       cont -> el
class Insertable     q m cont idx el cont' | q m cont idx el -> cont' where insert    :: InstModsX Insertable      q m cont -> idx -> el -> cont -> cont'
class Reservable     q m cont        cont' | q m cont        -> cont' where reserve   :: InstModsX Reservable      q m cont ->              cont -> cont'
class Releasable     q m cont idx    cont' | q m cont idx    -> cont' where release   :: InstModsX Releasable      q m cont -> idx ->       cont -> cont'

-- === Indexing ===
class TracksElems    q m cont        els   | q m cont        -> els   where elems     :: InstModsX TracksElems     q m cont ->              cont -> els
class TracksIxes     q m cont        ixes  | q m cont        -> ixes  where indexes   :: InstModsX TracksIxes      q m cont ->              cont -> ixes
class TracksFreeIxes q m cont        ixes  | q m cont        -> ixes  where freeIxes  :: InstModsX TracksFreeIxes  q m cont ->              cont -> ixes
class TracksUsedIxes q m cont        ixes  | q m cont        -> ixes  where usedIxes  :: InstModsX TracksUsedIxes  q m cont ->              cont -> ixes




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

--class (Container cont (IndexOf el cont) el) => Appendable cont     el where append :: el -> cont -> cont
--                                                                            default append :: Appendable' cont (IndexOf el cont) el => el -> cont -> cont
--                                                                            append = fst .: append'

--class Container cont idx el => Prependable         cont idx el where prepend'     :: el -> cont -> (cont, idx)
--class Container cont idx el => Updatable           cont idx el where update       :: idx -> el -> cont -> cont
--class Container cont idx el => Insertable          cont idx el where insert       :: idx -> el -> cont -> cont
--                                                                     unsafeInsert :: idx -> el -> cont -> cont


--class Requestable p where request :: p -> (ElementOf p, p)
--class Releasable  p where release :: ElementOf p -> p -> p

--instance {-# OVERLAPPABLE #-} Indexable cont idx el => Indexable'  cont idx el where unsafeIndex  = index
--instance {-# OVERLAPPABLE #-} Updatable cont idx el => Insertable' cont idx el where unsafeInsert = update

-- utils

--grow :: Growable cont => Int -> cont -> cont
--grow i cont = if i < 0 then error "negative index"
--                       else unsafeGrow i cont

--append :: Appendable' cont idx el => el -> cont -> cont
--append = fst .: append'

--prepend :: Prependable cont idx el => el -> cont -> cont
--prepend = fst .: prepend'

--alloc i = if i < 0 then error $ "negative length " <> show i
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


--instance Measurable a       => Measurable (Unchecked a) where size = size . unwrap
--instance Container a idx el => Container (Unchecked a) idx el where elems   = elems   . unwrap
--                                                                 indexes = indexes . unwrap

--index2' = index2 emptyOpts


--instance
--instance {-# OVERLAPPABLE #-} (t ~ (f -> g), opts ~ '[]) => FuncBuilder (OptBuilder opts f) (a -> b) where buildFunc = undefined

--instance (t ~ (Mods opts -> a -> b)) => FuncBuilder (OptBuilder opts t) (Mods opts -> a -> b) where buildFunc (OptBuilder f) = f Mods


extendOptBuilder :: Proxy opt -> OptBuilder opts a -> OptBuilder (opt ': opts) a
extendOptBuilder _ (OptBuilder a) = OptBuilder a

type ModConstraint mod = FuncTrans (mod ': opts) f g => OptBuilder opts f -> g


modConstraint :: Proxy mod -> ModConstraint mod
modConstraint = transFunc .: extendOptBuilder

unchecked :: ModConstraint Unchecked
unchecked = modConstraint (Proxy :: Proxy Unchecked)

safe :: ModConstraint Safe
safe = modConstraint (Proxy :: Proxy Safe)

ixed :: ModConstraint Ixed
ixed = modConstraint (Proxy :: Proxy Ixed)




















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

