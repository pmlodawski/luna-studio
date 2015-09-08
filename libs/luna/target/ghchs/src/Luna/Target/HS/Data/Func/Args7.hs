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
--{-# LANGUAGE PolyKinds #-}
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

data Arg (name :: NameStatus Symbol)

data ArgVal (name :: NameStatus Symbol) a = ArgVal a

data NameStatus a = Named a
                  | Unnamed
                  deriving (Show)

uArg :: a -> ArgVal Unnamed a
uArg = ArgVal

nArg :: a -> Proxy n -> ArgVal (Named n) a
nArg a _ = ArgVal a

empty = ()
addArg = (,)

-- === Signature args ===

newtype Provided a = Provided { fromProvided :: a }
data Unprovided = Unprovided

----------------------------------------------------------------------
-- Func
----------------------------------------------------------------------

data Func (sig :: [NameStatus Symbol]) f = Func { runFunc :: f } deriving (Show, Functor)

remapFunc :: Func sig f -> Func sig' f
remapFunc = Func . runFunc

simpleApp :: a -> Func (s ': ss) (a -> b) -> Func ss b
simpleApp a = remapFunc . fmap ($ a)

func :: Proxy sig -> f -> Func sig f
func _ = Func


----------------------------------------------------------------------
-- Args application
----------------------------------------------------------------------

-- === AppArg ===

class AppArg name a sig f sig' f' | name sig f -> a sig' f' where
    appArg :: ArgVal name a -> Func sig f -> Func sig' f'

instance (f~(Provided a -> f'), sig~(s ': sig')) => AppArg Unnamed a sig f sig' f' where
    appArg (ArgVal a) = remapFunc . fmap ($ Provided a)

instance (idx~IndexOf (Named n) sig, sig'~DeleteIdx idx sig, AppNth idx (Provided a) f f')
      => AppArg (Named n) a sig f sig' f' where
    appArg (ArgVal a) = remapFunc . fmap (appNth idx (Provided a)) where
        idx = (Proxy::Proxy idx)

-- === AppArgs ===

class AppArgs args sig f sig' f' | args sig f -> sig' f' where
    appArgsProto :: args -> Func sig f -> Func sig' f'

instance AppArgs () sig f sig f where
    appArgsProto _ = id

instance (a~ArgVal n v, AppArg n v sig f sig0 f0, AppArgs as sig0 f0 sig' f') => AppArgs (a,as) sig f sig' f' where
    appArgsProto (a,as) f = appArgsProto as (appArg a f)

appArgs = appArgsProto . reverse

-- === AppDefaults ===


class AppDefaults sig f f' | sig f -> f' where
    appDefaults :: Func sig f -> f'

instance AppDefaults '[] f f where
    appDefaults = runFunc

instance (f~(Unprovided -> f'), AppDefaults ss f' g)
      => AppDefaults (s ': ss) f g where
    appDefaults = appDefaults . simpleApp Unprovided


class AppDef m a arg n | m arg -> n where
    appDef :: m a -> arg -> n a

instance (arg~a) => AppDef m a (Provided (n arg)) n where
    appDef _ = fromProvided

instance (arg~a) => AppDef m a Unprovided m where
    appDef = const


noDef :: Provided a -> a
noDef = fromProvided

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


-- === DeleteIdx ===





-- === ExtractNth ===

class ExtractNth (num::Nat) t a t' | num t -> a t' where
    extractNth :: Proxy num -> t -> (a,t')

instance (t~(a,b)) => ExtractNth 0 t a b where
    extractNth _ = id

instance (b~(t1,t2), t~(t1,t0), ExtractNth (num - 1) t0 v t2) => ExtractNth num t v b where
    extractNth (_::Proxy n) (a,b) = (v, (a, b')) where
        (v, b') = extractNth (Proxy :: Proxy (n-1)) b

extractNth_ n t = snd $ extractNth n t


----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

tstx = appArgs (nArg [] (Proxy::Proxy "x"),())

tstArgs a = addArg (nArg 7 (Proxy::Proxy "x"))
          $ a

tstx2 v = appDefaults $ appArgs (tstArgs empty) v

--tstf1 (appDefault 0 -> a) (appDefault 0 -> b) = (a,b)
--tstFunc1 = func (Proxy::Proxy '[Named "x", Named "y"]) tstf1


--main = do
--    --print $ tstf1 (Provided []) (Provided "a")
--    --print $ appArgs (tstArgs empty) tstFunc1
--    print $ tstx2 tstFunc1
--    return ()


----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------

--class MemberProvider cls (name :: Symbol) argRep f | cls name argRep -> f where
--    getMember :: cls -> Proxy name -> argRep -> f



data V = V deriving (Show, Typeable)

type family SigOf cls (name :: Symbol) :: [NameStatus Symbol]

--instance Analyze a b => MemberProvider V "foo" args (a -> b) where
--    getMember _ _ _ = (\a -> monoType a)

--instance MemberProvider V "tst" args (a -> b -> (a,b)) where
--    getMember _ _ _ = tstf1

--member cls name args = getMember cls name (monoType args)

--mkFunc :: cls -> Proxy (name :: Symbol) -> f -> Func (SigOf cls name) f
--mkFunc _ _ = Func

--tst v = do
--    print $ member v (Proxy::Proxy "foo") ("a",()) "a"
--    print $ member v (Proxy::Proxy "foo") (5::Int,()) (5::Int)

--call' v sig args = appArgs args f
--    where rf = member v (Proxy::Proxy "tst") args
--          f  = func sig rf
--    --print 
--tsig = (npArg 15 (Proxy::Proxy "x"), (npArg [] (Proxy::Proxy "y"),()))

--main = do
--    print $ call' V tsig (tstArgs empty)
--    tst V
--    return ()




