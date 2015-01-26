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


{-# LANGUAGE DysfunctionalDependencies #-}


module Luna.Target.HS.Data.Func.Args6 where


import Prelude hiding (reverse)
import Unsafe.Coerce
import GHC.TypeLits
import Data.Typeable

import Data.PolyTypeable

import Luna.Target.HS.Utils.MonoType (monoType, TVar, Analyze)


import Unsafe.Coerce (unsafeCoerce)


----------------------------------------------------------------------
-- Arg
----------------------------------------------------------------------

newtype Arg (name :: NameStatus Symbol) a = Arg a deriving (Show)

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

uuArg :: Arg Unnamed Unprovided
uuArg = uArg Unprovided

upArg :: a -> Arg Unnamed (Provided a)
upArg = uArg . Provided

nuArg :: Proxy n -> Arg (Named n) Unprovided
nuArg = nArg Unprovided

npArg :: a -> Proxy n -> Arg (Named n) (Provided a)
npArg = nArg . Provided

----------------------------------------------------------------------
-- Func
----------------------------------------------------------------------

data Func sig f = Func { fromFunc :: (sig, f) } deriving (Show)

func :: sig -> f -> Func sig f
func a f = Func (a,f) 


----------------------------------------------------------------------
-- Args application
----------------------------------------------------------------------

-- === AppArg ===

class AppArg name a sig f sig' f' | name sig f -> a sig' f' where
    appArg :: Arg name a -> Func sig f -> Func sig' f'

instance (f~(a -> f'), sig~(s,sig')) => AppArg Unnamed a sig f sig' f' where
    appArg (Arg a) (Func ((_,sig'),f)) = func sig' (f a)

-- MatchDefArg a d => default arg has the same type as arg!
instance (idx~NameIndex n sig, AppNth idx a f f', ExtractNth idx sig d sig') => AppArg (Named n) a sig f sig' f' where
    appArg (Arg a) (Func (sig,f)) = func (extractNth_ idx sig) (appNth idx a f) where
        idx = (Proxy::Proxy idx)

-- === AppArgs ===

class AppArgs args sig f sig' f' | args sig f -> sig' f' where
    appArgsProto :: args -> Func sig f -> Func sig' f'

instance AppArgs () sig f sig f where
    appArgsProto _ = id

instance (a~Arg n v, AppArg n v sig f sig0 f0, AppArgs as sig0 f0 sig' f') => AppArgs (a,as) sig f sig' f' where
    appArgsProto (a,as) f = appArgsProto as (appArg a f)

class MatchDefArg v sarg
instance (v~d) => MatchDefArg v (Arg n (Provided d)) 
instance          MatchDefArg v (Arg n Unprovided) 

appArgs = appArgsProto . reverse

-- === AppDefaults ===

class AppDefaults sig f f' | sig f -> f' where
    appDefaults :: Func sig f -> f'

instance AppDefaults () f f where
    appDefaults (Func (_, f)) = f

instance (f~(a -> f'), AppDefaults as f' g, d~Arg n (Provided a)) => AppDefaults (d,as) f g where
    appDefaults (Func ((Arg (Provided a),as), f)) = appDefaults (func as $ f a)

type family ExtractNames a :: [NameStatus Symbol] where
    ExtractNames ()            = '[]
    ExtractNames (Arg n a, as) = n ': ExtractNames as


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


-- === DeleteNth ===

class DeleteNth (num::Nat) t t' | num t -> t' where
    deleteNth :: Proxy num -> t -> t'

instance (t~(a,b)) => DeleteNth 0 t b where
    deleteNth _ (_,b) = b

instance (DeleteNth (num-1) t2 t1, a~(t,t2), b~(t,t1)) => DeleteNth num a b where
    deleteNth (_::Proxy n) (a,b) = (a, deleteNth (Proxy :: Proxy (n-1)) b)


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

tstArgs a = addArg (uArg [])
          $ addArg (nArg 5 (Proxy::Proxy "x"))
          $ a

tstx2 v = appDefaults $ appArgs (tstArgs empty) v

tstf1 a b = (a,b)
tstFunc1 = func (npArg 15 (Proxy::Proxy "x"), (npArg [] (Proxy::Proxy "y"),())) tstf1





type family NameIndex (a :: Symbol) as :: Nat where
    NameIndex n (Arg (Named n) a, as) = 0
    NameIndex n (a, as)                  = NameIndex n as + 1


----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------

class MemberProvider cls (name :: Symbol) argRep f | cls name argRep -> f where
    getMember :: cls -> Proxy name -> argRep -> f



data V = V deriving (Show, Typeable)

instance Analyze a b => MemberProvider V "foo" args (a -> b) where
    getMember _ _ _ = (\a -> monoType a)

instance MemberProvider V "tst" args (a -> b -> (a,b)) where
    getMember _ _ _ = tstf1

member cls name args = getMember cls name (monoType args)


tst v = do
    print $ member v (Proxy::Proxy "foo") ("a",()) "a"
    print $ member v (Proxy::Proxy "foo") (5::Int,()) (5::Int)

call' v sig args = appArgs args f
    where rf = member v (Proxy::Proxy "tst") args
          f  = func sig rf
    --print 
tsig = (npArg 15 (Proxy::Proxy "x"), (npArg [] (Proxy::Proxy "y"),()))

main = do
    print $ call' V tsig (tstArgs empty)
    tst V
    return ()


class AppDefault a arg where
    appDefault :: a -> arg -> a

instance (arg~a) => AppDefault a (Provided arg) where
    appDefault _ = fromProvided

instance (arg~a) => AppDefault a Unprovided where
    appDefault = const

