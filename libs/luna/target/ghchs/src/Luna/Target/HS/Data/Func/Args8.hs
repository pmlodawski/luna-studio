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


module Luna.Target.HS.Data.Func.Args8 where


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

uuArg :: Arg Unnamed Unprovided
uuArg = uArg Unprovided

upArg :: a -> Arg Unnamed (Provided a)
upArg = uArg . Provided

nuArg :: Proxy n -> Arg (Named n) Unprovided
nuArg = nArg Unprovided

npArg :: a -> Proxy n -> Arg (Named n) (Provided a)
npArg = nArg . Provided

empty = ()
addArg = (,)

-- === Signature args ===

newtype Provided a = Provided { fromProvided :: a }
data Unprovided = Unprovided

----------------------------------------------------------------------
-- Func
----------------------------------------------------------------------

data Func (kind :: Nat) sig f = Func { fromFunc :: (sig,f) } deriving (Show, Functor)

func :: sig -> f -> Func (ArgsKind sig) sig f
func sig f = Func (sig,f)

runFunc = snd . fromFunc

--remapFunc :: Func sig f -> Func sig' f
--remapFunc = Func . fromFunc

--simpleApp :: a -> Func (s ': ss) (a -> b) -> Func ss b
--simpleApp a = remapFunc . fmap ($ a)

--func :: Proxy sig -> f -> Func sig f
--func _ = Func

class DeleteNthC (num::Nat) t t' | num t -> t' where
    deleteNth :: Proxy num -> t -> t'

instance (t~(a,b)) => DeleteNthC 0 t b where
    deleteNth _ (_,b) = b

instance (DeleteNthC (num-1) t2 t1, a~(t,t2), b~(t,t1)) => DeleteNthC num a b where
    deleteNth (_::Proxy n) (a,b) = (a, deleteNth (Proxy :: Proxy (n-1)) b)


type family NameIndex (a :: Symbol) as :: Nat where
    NameIndex n (Arg (Named n) a,as) = 0
    NameIndex n (a,as)               = NameIndex n as + 1

----------------------------------------------------------------------
-- Args application
----------------------------------------------------------------------

-- === AppArg ===

class AppArg name a sig f sig' f' | name f -> a f', name sig -> sig' where
    appArg :: Arg name a -> Func k sig f -> Func (k-1) sig' f'

instance (f~(a -> f'), sig~(s,sig')) => AppArg Unnamed a sig f sig' f' where
    appArg (Arg a) (Func ((s,ss),f)) = Func (ss, f a)

instance (idx~NameIndex n sig, AppNth idx a f f', DeleteNthC idx sig sig')
      => AppArg (Named n) a sig f sig' f' where
    appArg (Arg a) (Func (sig,f)) = Func (deleteNth idx sig, appNth idx a f) where
        idx = (Proxy::Proxy idx)

---- === AppArgs ===

--class AppArgs args sig f sig' f' | args sig f -> sig' f' where
--    appArgsProto :: args -> Func k sig f -> Func (k - (ArgsKind args)) sig' f'

--instance AppArgs () sig f sig f where
--    appArgsProto _ = id

--instance (a~Arg n v, AppArg n v sig f sig0 f0, AppArgs as sig0 f0 sig' f') => AppArgs (a,as) sig f sig' f' where
--    appArgsProto (a,as) f = appArgsProto as (appArg a f)

-----------------------------


class AppArgs args k sig f k' sig' f' | args sig f -> sig' f', k args -> k' where
    appArgsProto :: args -> Func k sig f -> Func k' sig' f'

instance AppArgs () k sig f k sig f where
    appArgsProto _ = id

instance (a~Arg n v, AppArg n v sig f sig0 f0, AppArgs as (k-1) sig0 f0 k' sig' f')
      => AppArgs (a,as) k sig f k' sig' f' where
    appArgsProto (a,as) f = appArgsProto as (appArg a f)


-- === AppDefaults ===

class AppDefaults k f sig f' | k f -> sig f' where
    appDefaults :: Func k sig f -> f'

instance (sig~()) => AppDefaults 0 f () f where
    appDefaults = runFunc

instance (f~(a->f'), sig~(Arg n (Provided a),as), AppDefaults (k-1) f' as g) => AppDefaults k f sig g where
    appDefaults (Func ((Arg (Provided a),as),f)) = appDefaults $ (Func (as, f a) :: Func (k-1) as f')


type family ArgsKind a :: Nat where
    ArgsKind ()     = 0
    ArgsKind (a,as) = 1 + ArgsKind as

--class AppDefaults k f sig f' | k f -> sig f' where
--    appDefaults :: Func k sig f -> f'

--instance AppDefaults 0 f () f where
--    appDefaults = runFunc

--instance (f~(a->f'), AppDefaults (n-1) f' as g) => AppDefaults n f (a,as) g where
--    appDefaults (Func ((a,as),f)) = appDefaults $ (Func (as, f a) :: Func (n-1) as f')

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



----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

--appArgs = appArgsProto . reverse

--foo = appArgs (nArg [] (Proxy::Proxy "x"),())

--foo2 f = func (npArg [] (Proxy::Proxy "x"),()) f

--foo3 f = appArg (nArg [] (Proxy::Proxy "x")) $ foo2 f

--bar2 f = appDefaults (func (npArg [] (Proxy::Proxy "x"),()) f)

--sig = addArg uuArg
--    $ addArg (npArg [] (Proxy::Proxy "x"))
--    $ empty

--args = addArg (uArg 5)
--     $ addArg (nArg [] (Proxy::Proxy "x"))
--     $ empty

--tst a b = (a,b)

--ftst f = appDefaults $ appArgs args f

--main = do
--    print $ ftst (func (npArg [] (Proxy::Proxy "x"),(npArg [] (Proxy::Proxy "y"),())) tst)
--    return ()


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




