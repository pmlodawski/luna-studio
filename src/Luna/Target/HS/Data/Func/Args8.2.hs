{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ViewPatterns #-}


{-# LANGUAGE DysfunctionalDependencies #-}


module Luna.Target.HS.Data.Func.Args7 where


import Prelude hiding (reverse)
import Unsafe.Coerce
import GHC.TypeLits
import Data.Typeable

import Data.PolyTypeable

import Luna.Target.HS.Utils.MonoType (monoType, TVar, Analyze)
import Type.List (IndexOf, DeleteIdx)

import Unsafe.Coerce (unsafeCoerce)


----------------------------------------------------------------------
-- Arg
----------------------------------------------------------------------

data Arg (name :: NameStatus Symbol) a = Arg a

data NameStatus a = Named a
                  | Unnamed
                  deriving (Show)

uArg :: a -> Arg Unnamed a
uArg = Arg

nArg :: a -> Proxy n -> Arg (Named n) a
nArg a _ = Arg a

empty = ()
addArg = (,)

-- === Signature args ===

newtype Provided a = Provided { fromProvided :: a }
data Unprovided = Unprovided

----------------------------------------------------------------------
-- Func
----------------------------------------------------------------------

data Func (kind :: Nat) (sig :: [NameStatus Symbol]) defs f = Func { fromFunc :: (defs,f) } deriving (Show, Functor)

func :: (Length sig ~ Length defs) => Proxy sig -> defs -> f -> Func (Length sig) sig defs f
func _ defs f = Func (defs,f)

--runFunc = snd . fromFunc

----remapFunc :: Func sig f -> Func sig' f
----remapFunc = Func . fromFunc

----simpleApp :: a -> Func (s ': ss) (a -> b) -> Func ss b
----simpleApp a = remapFunc . fmap ($ a)

----func :: Proxy sig -> f -> Func sig f
----func _ = Func

class DeleteNthC (num::Nat) t t' | num t -> t' where
    deleteNth :: Proxy num -> t -> t'

instance (t~(a,b)) => DeleteNthC 0 t b where
    deleteNth _ (_,b) = b

instance (DeleteNthC (num-1) t2 t1, a~(t,t2), b~(t,t1)) => DeleteNthC num a b where
    deleteNth (_::Proxy n) (a,b) = (a, deleteNth (Proxy :: Proxy (n-1)) b)


--type family NameIndex (a :: Symbol) as :: Nat where
--    NameIndex n (Arg (Named n) a,as) = 0
--    NameIndex n (a,as)               = NameIndex n as + 1

------------------------------------------------------------------------
---- Args application
------------------------------------------------------------------------

---- === AppArg ===

class AppArg name a sig defs f sig' defs' f' | name sig f -> a f' sig'
                                             , f -> defs
                                             , defs name sig -> defs' where
    appArg :: Arg name a -> Func k sig defs f -> Func (k-1) sig' defs' f'

instance (f~(a -> f'), sig~(s ': sig'), defs~(d,defs'))
      => AppArg Unnamed a sig defs f sig' defs' f' where
    appArg (Arg a) (Func ((s,ss),f)) = Func (ss, f a)

instance ( idx~IndexOf (Named n) sig, AppNth idx a f f', sig'~DeleteIdx idx sig
         , DeleteNthC idx defs defs')
      => AppArg (Named n) a sig defs f sig' defs' f' where
    appArg (Arg a) (Func (sig,f)) = Func (deleteNth idx sig, appNth idx a f) where
        idx = (Proxy::Proxy idx)

---- === AppArgs ===

--class AppArgs args sig f sig' f' | args sig f -> sig' f' where
--    appArgsProto :: args -> Func k sig f -> Func (k - (Length args)) sig' f'

--instance AppArgs () sig f sig f where
--    appArgsProto _ = id

--instance (a~Arg n v, AppArg n v sig f sig0 f0, AppArgs as sig0 f0 sig' f') => AppArgs (a,as) sig f sig' f' where
--    appArgsProto (a,as) f = appArgsProto as (appArg a f)

-----------------------------

appArgs = appArgsProto . reverse

foo = appArgs (nArg [] (Proxy::Proxy "x"),())

--foo3 :: f -> Func 1 ([a],()) f
foo3 f = func (Proxy :: Proxy) ([],()) f

--foo4 f = appArg (nArg [] (Proxy::Proxy "x")) $ foo3 f


class AppArgs args k sig defs f k' sig' defs' f' | args sig f -> sig' f'
                                                 , args k -> k'
                                                 , f -> defs where
    appArgsProto :: args -> Func k sig defs f -> Func k' sig' defs' f'

instance AppArgs () k sig defs f k sig defs f where
    appArgsProto _ = id

instance (a~Arg n v, AppArg n v sig defs f sig0 defs0 f0, AppArgs as (k-1) sig0 defs0 f0 k' sig' defs' f')
      => AppArgs (a,as) k sig defs f k' sig' defs' f' where
    appArgsProto (a,as) f = appArgsProto as (appArg a f)



-- === AppDefaults ===


--foo2 f = appDefaults (func ([],()) f)

--class AppDefaults k f sig f' | k f -> sig f' where
--    appDefaults :: Func k sig f -> f'

--instance (sig~()) => AppDefaults 0 f () f where
--    appDefaults = runFunc

--instance (f~(a->f'), sig~(a,as), AppDefaults (n-1) f' as g) => AppDefaults n f sig g where
--    appDefaults (Func ((a,as),f)) = appDefaults $ (Func (as, f a) :: Func (n-1) as f')


type family Length (a::k) :: Nat where
    Length ()        = 0
    Length (a,as)    = 1 + Length as
    Length '[]       = 0
    Length (a ': as) = 1 + Length as


--class AppDefaults k f sig f' | k f -> sig f' where
--    appDefaults :: Func k sig f -> f'

--instance AppDefaults 0 f () f where
--    appDefaults = runFunc

--instance (f~(a->f'), AppDefaults (n-1) f' as g) => AppDefaults n f (a,as) g where
--    appDefaults (Func ((a,as),f)) = appDefaults $ (Func (as, f a) :: Func (n-1) as f')




--instance (f~(Unprovided -> f'), AppDefaults ss f' g)
--      => AppDefaults (s ': ss) f g where
--    appDefaults = appDefaults . simpleApp Unprovided


--class AppDef m a arg n | m arg -> n where
--    appDef :: m a -> arg -> n a

--instance (arg~a) => AppDef m a (Provided (n arg)) n where
--    appDef _ = fromProvided

--instance (arg~a) => AppDef m a Unprovided m where
--    appDef = const


--noDef :: Provided a -> a
--noDef = fromProvided

----------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------

-- === Reverse ===

class Reverse t r t' | t r -> t' where
    rev :: t -> r -> t'

instance Reverse () a a where
    rev _ = id

instance Reverse xs (x,a) b => Reverse (x,xs) a b where
    rev (x,xs) a = rev xs (x,a)

reverse a = rev a ()


-- === AppNth ===

class AppNth (k::Nat) a f g | k f -> a g where
    appNth :: Proxy k -> a -> f -> g

instance (f~(a -> b)) => AppNth 0 a f b where
    appNth _ a = ($a)

instance (CmpNat n 0 ~ ok, AppNth' ok n a f g)
      => AppNth n a f g where
    appNth = appNth' (Proxy :: Proxy ok)

class AppNth' (ok :: Ordering) (k::Nat) a f g | k f -> a g where
    appNth' :: Proxy ok -> Proxy k -> a -> f -> g

instance (f~(t -> s), g~(t -> u), AppNth (n-1) a s u) 
      => AppNth' GT n a f g where
    appNth' _ _ a = (\f t -> appNth (Proxy :: Proxy (n-1)) a (f t))


---- === DeleteIdx ===





---- === ExtractNth ===

--class ExtractNth (num::Nat) t a t' | num t -> a t' where
--    extractNth :: Proxy num -> t -> (a,t')

--instance (t~(a,b)) => ExtractNth 0 t a b where
--    extractNth _ = id

--instance (b~(t1,t2), t~(t1,t0), ExtractNth (num - 1) t0 v t2) => ExtractNth num t v b where
--    extractNth (_::Proxy n) (a,b) = (v, (a, b')) where
--        (v, b') = extractNth (Proxy :: Proxy (n-1)) b

--extractNth_ n t = snd $ extractNth n t


------------------------------------------------------------------------
---- Tests
------------------------------------------------------------------------

--tstx = appArgs (nArg [] (Proxy::Proxy "x"),())

--tstArgs a = addArg (nArg 7 (Proxy::Proxy "x"))
--          $ a

--tstx2 v = appDefaults $ appArgs (tstArgs empty) v

----tstf1 (appDefault 0 -> a) (appDefault 0 -> b) = (a,b)
----tstFunc1 = func (Proxy::Proxy '[Named "x", Named "y"]) tstf1


----main = do
----    --print $ tstf1 (Provided []) (Provided "a")
----    --print $ appArgs (tstArgs empty) tstFunc1
----    print $ tstx2 tstFunc1
----    return ()


------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------

----class MemberProvider cls (name :: Symbol) argRep f | cls name argRep -> f where
----    getMember :: cls -> Proxy name -> argRep -> f



--data V = V deriving (Show, Typeable)

--type family SigOf cls (name :: Symbol) :: [NameStatus Symbol]

----instance Analyze a b => MemberProvider V "foo" args (a -> b) where
----    getMember _ _ _ = (\a -> monoType a)

----instance MemberProvider V "tst" args (a -> b -> (a,b)) where
----    getMember _ _ _ = tstf1

----member cls name args = getMember cls name (monoType args)

----mkFunc :: cls -> Proxy (name :: Symbol) -> f -> Func (SigOf cls name) f
----mkFunc _ _ = Func

----tst v = do
----    print $ member v (Proxy::Proxy "foo") ("a",()) "a"
----    print $ member v (Proxy::Proxy "foo") (5::Int,()) (5::Int)

----call' v sig args = appArgs args f
----    where rf = member v (Proxy::Proxy "tst") args
----          f  = func sig rf
----    --print 
----tsig = (npArg 15 (Proxy::Proxy "x"), (npArg [] (Proxy::Proxy "y"),()))

----main = do
----    print $ call' V tsig (tstArgs empty)
----    tst V
----    return ()




