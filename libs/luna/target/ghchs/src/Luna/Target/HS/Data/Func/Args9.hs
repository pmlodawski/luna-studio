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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
--{-# LANGUAGE ImpredicativeTypes #-}


{-# LANGUAGE DysfunctionalDependencies #-}


module Luna.Target.HS.Data.Func.Args9 where


import Prelude hiding (reverse)
import Unsafe.Coerce
import GHC.TypeLits
import Data.Typeable

import Luna.Target.HS.Utils.MonoType (monoType, TVar, Analyze)
import Type.List (IndexOf, DeleteIdx)

import Unsafe.Coerce (unsafeCoerce)

--import Control.Monad.Identity

----------------------------------------------------------------------
-- Arg
----------------------------------------------------------------------

newtype Arg (name :: NameStatus Symbol) a = Arg a deriving (Show)

data NameStatus a = Named a
                  | Unnamed
                  deriving (Show)

newtype Provided a = Provided { fromProvided :: a } deriving (Show)
data Unprovided = Unprovided deriving (Show)

empty = ()
addArg = (,)

-- === Utils ===

uArg :: a -> Arg Unnamed a
uArg = Arg

nArg :: a -> Proxy n -> Arg (Named n) a
nArg a _ = Arg a

-- === Arg constructors ===

uuArg :: Arg Unnamed (Hole a)
uuArg = uArg Hole

upArg :: a -> Arg Unnamed (Provided a)
upArg = uArg . Provided

nuArg :: Proxy n -> Arg (Named n) (Hole a)
nuArg = nArg Hole

npArg :: a -> Proxy n -> Arg (Named n) (Provided a)
npArg = nArg . Provided


uuSigArg :: Arg Unnamed Unprovided
uuSigArg = uArg Unprovided

upSigArg :: a -> Arg Unnamed (Provided a)
upSigArg = uArg . Provided

nuSigArg :: Proxy n -> Arg (Named n) Unprovided
nuSigArg = nArg Unprovided

npSigArg :: Proxy n -> a -> Arg (Named n) (Provided a)
npSigArg = flip (nArg . Provided)


simpleFSig0 = (nuSigArg (Proxy::Proxy "self"), ())
simpleFSig1 = (nuSigArg (Proxy::Proxy "self"), (uuSigArg,()))

----------------------------------------------------------------------
-- Func
----------------------------------------------------------------------

newtype Func (kind :: Nat) sig f = Func { fromFunc :: (sig,f) } deriving (Show, Functor)

func :: sig -> f -> Func (ArgsKind sig) sig f
func sig f = Func (sig,f)

runFunc :: Func k s f -> f
runFunc = snd . fromFunc



class DeleteNthC (num::Nat) t t' | num t -> t' where
    deleteNth :: Proxy num -> t -> t'

instance (t~(a,b)) => DeleteNthC 0 t b where
    deleteNth _ (_,b) = b

instance (DeleteNthC (num-1) t2 t1, a~(t,t2), b~(t,t1)) => DeleteNthC num a b where
    deleteNth (_::Proxy n) (a,b) = (a, deleteNth (Proxy :: Proxy (n-1)) b)


class ExtractNthC (num::Nat) t a t' | num t -> a t' where
    extractNth :: Proxy num -> t -> (a,t')

instance t~(a,b) => ExtractNthC 0 t a b where
    extractNth _ = id

instance (t~(x,y), b~(x,t0), ExtractNthC (n - 1) y a t0) => ExtractNthC n t a b where
    extractNth _ (a,b) = (x, (a,b'))
        where (x,b') = extractNth (Proxy :: Proxy (n-1)) b


type family NameIndex (a :: Symbol) as :: Nat where
    NameIndex n (Arg (Named n) a,as) = 0
    NameIndex n (a,as)               = NameIndex n as + 1

----------------------------------------------------------------------
-- Args application
----------------------------------------------------------------------

-- === AppArg ===

data Hole a = Hole

class AppArg name pr a sig f sig' f' | name f -> a f', name sig -> sig' where
    appArg :: Arg name (pr a) -> Func k sig f -> Func (k-1) sig' f'

instance (f~(a -> f'), sig~(s,sig')) => AppArg Unnamed Provided a sig f sig' f' where
    appArg (Arg (Provided a)) (Func ((s,ss),f)) = Func (ss, f a)

instance (f~(a -> f'), sig~(Arg dn (Provided a),sig')) => AppArg Unnamed Hole a sig f sig' f' where
    appArg _ (Func ((Arg (Provided a),ss),f)) = Func (ss, f a)

instance (idx~NameIndex n sig, AppNth idx a f f', DeleteNthC idx sig sig')
      => AppArg (Named n) Provided a sig f sig' f' where
    appArg (Arg (Provided a)) (Func (sig,f)) = Func (deleteNth idx sig, appNth idx a f) where
        idx = (Proxy::Proxy idx)

instance (idx~NameIndex n sig, AppNth idx a f f', ExtractNthC idx sig a sig')
      => AppArg (Named n) Hole a sig f sig' f' where
    appArg _ (Func (sig,f)) = Func (sig', appNth idx a f) where
        (a,sig') = extractNth idx sig
        idx = (Proxy::Proxy idx)


-------------------------------


class AppArgs args k sig f k' sig' f' | args sig f -> sig' f', k args -> k' where
    appArgsProto :: args -> Func k sig f -> Func k' sig' f'

instance AppArgs () k sig f k sig f where
    appArgsProto _ = id

instance (a~(Arg n (pr v)), AppArg n pr v sig f sig0 f0, AppArgs as (k-1) sig0 f0 k' sig' f')
      => AppArgs (a,as) k sig f k' sig' f' where
    appArgsProto (a,as) f = appArgsProto as (appArg a f)

appArgs = appArgsProto . reverse


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


------------------------------------------------------------------------
---- Utils
------------------------------------------------------------------------

-- === Reverse ===

class Reverse' t r t' | t r -> t' where
    rev :: t -> r -> t'

instance Reverse' () a a where
    rev _ = id

instance Reverse' xs (x,a) b => Reverse' (x,xs) a b where
    rev (x,xs) a = rev xs (x,a)

type Reverse t t' = Reverse' t () t'

reverse :: Reverse t t' => t -> t'
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

--foo_sa_1 = appArg (uArg (Provided []))
--foo_sa_2 = appArg (nArg (Provided []) (Proxy::Proxy "x"))



--foo = appArgs (nArg (Provided []) (Proxy::Proxy "x"),())

--foo2 f = func (npArg [] (Proxy::Proxy "x"),()) f

--foo3 f = appArg (npArg [] (Proxy::Proxy "x")) $ foo2 f

--bar2 f = appDefaults (func (npArg [] (Proxy::Proxy "x"),()) f)

--sig = addArg uuArg
--    $ addArg (npArg [] (Proxy::Proxy "x"))
--    $ empty

--args = addArg (upArg 5)
--     $ addArg (uArg Hole)
--     $ empty

--args2 = addArg (upArg 6)
--      $ addArg (npArg [] (Proxy::Proxy "y"))
--      $ empty

--args3 = addArg (upArg 7)
--      $ addArg (npArg [] (Proxy::Proxy "x"))
--      $ empty

--args4 = addArg (upArg 8)
--      $ empty

--tst a b = (a,b)

--callTst f = appDefaults $ appArgs args f

--fTst = func (npArg [] (Proxy::Proxy "x"),(npArg [] (Proxy::Proxy "y"),())) tst

--main = do
--    print $ callTst fTst
--    print $ appDefaults $ appArgs args2 fTst
--    print $ appDefaults $ appArgs args3 fTst
--    print $ appDefaults $ appArgs args4 fTst
--    return ()


--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

--class MemberProvider cls (name :: Symbol) argRep f | cls name argRep -> f where
--    getMember :: cls -> Proxy name -> argRep -> f



--data V = V deriving (Show, Typeable)


--instance Analyze a b => MemberProvider V "foo" args (a -> a) where
--    getMember _ _ _ = (\a -> a)

--member cls name args = getMember cls name (monoType args)

--instance MemberProvider V "bar" args (a -> [bx]) where
--    getMember _ _ _ = (\a -> [])

--tst v = do
--    print $ member v (Proxy::Proxy "foo") ("a",()) "a"
--    print $ member v (Proxy::Proxy "foo") (5::Int,()) (5::Int)

--tsig = (npArg 15 (Proxy::Proxy "x"), (npArg [] (Proxy::Proxy "y"),()))

--main = do
--    tst V
--    print $ member V (Proxy::Proxy "bar") [] 4 ++ [5]
--    print $ member V (Proxy::Proxy "bar") [] 4 ++ ["a"]
--    --print $ monoType []
--    return ()




--newtype LamH f args = LamH (f,args)

--class Call h f args a | h f args -> a where
--    call :: h f args -> a

--instance LamH f args a where
--    call (LamH (f,args)) =

----tst f = (f 5, f "a")

--data Hidden = Hidden



----newtype Lam def monoDef = Lam (def, monoDef)

----bar :: Lam def monodef -> Proxy monodef
----bar _ = Proxy

----lam :: Analyze def monodef => def -> Lam def monodef
----lam a = Lam (a, monoType a)


----class Foo a where
----    foo :: a -> Int

----instance Foo Int where
----    foo _ = 1

----instance Foo String where
----    foo _ = 2

----appLam (Lam f, mf) 

--newtype Lam f = Lam {runLam :: f}

----data Lam1

----foo lam = (runLam lam 1, runLam lam "a")

--main = do
--    let l = Lam id :: Lam (a -> a)
--    print $ runLam l "a"
--    print $ runLam l 5

--    --let foo :: Identity (forall a. a -> a) -> (Integer, String); foo lam = (runIdentity lam 1, runIdentity lam "a") in foo
--    --let l = Lam id :: Lam (forall a. a -> a)
--    --print $ foo l
--    return ()
