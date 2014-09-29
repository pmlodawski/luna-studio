{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

--{-# LANGUAGE IncoherentInstances #-}

{-# LANGUAGE DeriveDataTypeable #-}

!{-# LANGUAGE RightSideContexts #-}
{-# LANGUAGE DysfunctionalDependencies #-}


{-# LANGUAGE RebindableSyntax #-}


module Vector where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.PolyApplicative
import Control.PolyApplicative.App
import Luna.Target.HS.Control.Context
import Luna.Target.HS.Control.Error
import Luna.Target.HS.Control.Flow
import Luna.Target.HS.Utils.BaseMonads
import Luna.Target.HS.Data.Func
import Control.Monad.Morph
import Flowbox.Utils
import Data.Typeable (Typeable, Proxy(..))
import Data.TypeLevel
import Data.Wrap
import Luna.Target.HS.Data.Struct.Prop
import Luna.Target.HS.Control.Context.Rebindable
import GHC.TypeLits (Symbol)
import Data.TupleList
import Luna.Target.HS.Control.Flow.Utils

import Luna.Target.HS.Control.Context.Pipe3 hiding (main)

import TstX


instance Pipe3 (Req req (MonadCtx env1 set1 m1)) (MonadCtx env2 set2 m2) <= (env1~env2, m1~m2, set1~set2) where
    pipe3 f = f . Req


instance Pipe3 (Req req (MonadCtx env1 set1 m1)) (Req req2 (MonadCtx env2 set2 m2)) <= (req~req2, env1~env2, m1~m2, set1~set2) where
    pipe3 = ($)

instance Pipe3 (Req req (MonadCtx env set m1)) (Value m2) <= (env~m2, set~Insert req Empty, m1~t m2, MonadTrans t, Monad m2) where
    pipe3 f = f . Req . MonadCtx . lift . fromValue

    --pipe3 f = f . matchReqMonadCtx . lift . fromValue


instance Pipe3 m1 (MonadCtx envout set2 m2) <= (m1 ~ MonadCtx envout set2 m2) where
    pipe3 = ($)

instance Pipe3 m1 (Value m2) <= (m1 ~ Value m2) where
    pipe3 = ($)

---


--instance Pipe4 (Req req (MonadCtx env1 set1 m1) a1 -> b) (MonadCtx env2 set2 m2 a2) b <= (a1~a2, env1~env2, m1~m2, set1~set2) where
--    pipe4 f = f . Req

--instance Pipe4 (Req req (MonadCtx env1 set1 m1) a1 -> b) (Req req2 (MonadCtx env2 set2 m2) a2) b <= (a1~a2, req~req2, env1~env2, m1~m2, set1~set2) where
--    pipe4 = ($)

--instance Pipe4 (Req req (MonadCtx env set m1) a1 -> b) (Value m2 a2) b <= (a1~a2, env~m2, set~Insert req Empty, m1~t m2, MonadTrans t, Monad m2) where
--    pipe4 f = f . Req . MonadCtx . lift . fromValue

--instance Pipe4 func (MonadCtx envout set2 m2 a2) b <= (func~(m1 a1 -> b), a1~a2, m1 ~ MonadCtx envout set2 m2) where
--    pipe4 = ($)

--instance Pipe4 func (Value m2 a2) b <= (func~(m1 a1 -> b), a1~a2, m1 ~ Value m2) where
--    pipe4 = ($)



--instance Pipe3 m1 m2 <= (m1~m2) where
--    pipe3 = ($)

            --class MkMonadCtx m1 req env set m | m1 req -> env set m where
            --    mkMonadCtx :: m1 a -> Req req (MonadCtx env set m) a

            --instance MkMonadCtx (MonadCtx env set m) req env set m where
            --    mkMonadCtx = Req

            --instance MkMonadCtx (Value m) req m set (t m) <= (m1~t m, set~Insert req Empty, MonadTrans t, Monad m) where
            --    mkMonadCtx = matchReqMonadCtx . lift . fromValue

            ----runStateTX2 x = mkMonadCtx x
            --runStateTX2 x a = runStateTX' (mkMonadCtx x) a

class TuplePipe f a out | f a -> out where
    tuplePipe :: f -> a -> out


--class ApplyMonad 

instance TuplePipe f () f where
    tuplePipe = const

instance TuplePipe (w -> b) (m2 (s a2),xs) out <= (w~m1 (s a1), a1~a2, Pipe3 m1 m2, TuplePipe b xs out) where
    tuplePipe f (x,xs) = (f `pipe3` x) `tuplePipe` xs


class TupleApp f a out | f a -> out where
    tupleApp :: f -> a -> out

instance TupleApp f () f where
    tupleApp = const

instance TupleApp f (a,xs) out <= (f~(a->b), TupleApp b xs out) where
    tupleApp f (x,xs) = (f x) `tupleApp` xs


class TupleApp2 f a out | f a -> out where
    tupleApp2 :: f -> a -> out

instance TupleApp2 f () out <= out~f where
    tupleApp2 = const

instance TupleApp2 f (a1,xs) out <= (f~(a2->b), a1~ma1(va1), a2~ma2(va2), va1~va2, Pipe3 ma2 ma1, TupleApp2 b xs out) where
    tupleApp2 f (x,xs) = (f `pipe3` x) `tupleApp2` xs


class TupleApp3 f a out | f a -> out where
    tupleApp3 :: f -> a -> out

instance TupleApp3 f () out <= out~f where
    tupleApp3 = const

instance TupleApp3 f (a1,xs) out <= (Pipe4 f a1 f0, TupleApp3 f0 xs out) where
    tupleApp3 f (x,xs) = (f `pipe4` x) `tupleApp3` xs

--instance TuplePipe func arg out <= (func~(w -> b), w~m1 (s1 a1), arg~(m2 (s2 a2),xs), s1~s2, a1~a2, Pipe3 m1 m2, TuplePipe b xs out) where
--    tuplePipe f (x,xs) = (f `pipe3` x) `tuplePipe` xs


--class TuplePipe2 f a out | f a -> out where
--    tuplePipe2 :: f -> a -> out

--instance TuplePipe f () f where
--    tuplePipe = const

--instance TuplePipe f (m2 a,xs) out <= (f~(m1 a -> b), Pipe2 m1 m2, TuplePipe b xs out) where
--    tuplePipe f (x,xs) = (f `pipe2` x) `tuplePipe` xs

-------------------------------------------------------------------------------

--class PropProto (name :: Symbol) fptr | name -> fptr where
--    propProto :: Proxy name -> fptr

--prop = propBind . propProto

newtype PropBind prop base = PropBind (prop,base) deriving (Show, Eq, Typeable)

propBind = curry PropBind

ofType :: a -> a -> a
ofType = const

u = undefined

p = Proxy
type P = Proxy

(~::) = ofType

instance Func (PropBind prop base) args out <= Func prop (base, args) out where
    getFunc (PropBind (prop, base)) args = getFunc prop (base,args)

--instance Func2 (PropBind prop base) args out <= Func2 prop (base, args) out where
--    getFunc2 (PropBind (prop, base)) args = getFunc2 prop (base,args)

-------------------------------------------------------------------------------

--class Vector a:
--    x,y,z :: a = 0
--    def foo a b=0: a+b
--    def bar a b: (a,b)
--    def baz x: runState @x

data Vector a = Vector { _prop_Vector_x :: a
                       , _prop_Vector_y :: a
                       , _prop_Vector_z :: a
                       } deriving (Show, Eq, Typeable)

data Prop_Vector_x     = Prop_Vector_x     deriving (Show, Eq, Typeable)
data Prop_Vector_y     = Prop_Vector_y     deriving (Show, Eq, Typeable)
data Prop_Vector_z     = Prop_Vector_z     deriving (Show, Eq, Typeable)
data Prop_Vector_x_set = Prop_Vector_x_set deriving (Show, Eq, Typeable)
data Prop_Vector_y_set = Prop_Vector_y_set deriving (Show, Eq, Typeable)
data Prop_Vector_z_set = Prop_Vector_z_set deriving (Show, Eq, Typeable)


cons_Vector_func2 (x,(y,(z,()))) = liftF3 Vector x y z

--cons_Vector_func22 (x,(y,(z,()))) = (liftF3 Vector) `pipe3` x `pipe3` y `pipe3` z


data VVV a = VVV a

cons_VVV = liftF1 VVV


--cons_VVV2 a = liftF1 VVV `pipe3` a

--cons_VVV2' a = (flip runStateTX' (0::Int)) `pipe3` a

data Cons_Vector = Cons_Vector deriving (Show, Eq, Typeable)
data Cons_Vector2 = Cons_Vector2 deriving (Show, Eq, Typeable)
cons_Vector = appH Cons_Vector $ (mkArg (val (0::Int)) ~:: (u :: NDParam "x" a)) // (mkArg (val 0) :: NDParam "y" (Value Pure(Safe Int))) // (mkArg (val 0) :: NDParam "z" (Value Pure(Safe Int))) // ()

--instance Func Cons_Vector args out <= (args~(a,(a,(a,()))), out~(Value Pure(Safe(Vector a)))) where
--    getFunc _ = cons_Vector_func

instance (PolyApplicative (Value Pure) m7 m4,
      PolyApplicative Safe m12 m10, PolyApplicative m10 m11 m6,
      PolyApplicative m6 m8 m9, PolyApplicative m4 m5 m1,
      PolyApplicative m1 m2 m3,
      args~(m7 (m12 a), (m5 (m11 a), (m2 (m8 a), ()))),
      out~m3 (m9 (Vector a))) =>
    Func Cons_Vector args out where
    getFunc _ = cons_Vector_func2


instance (PolyApplicative (Value Pure) m7 m4,
      PolyApplicative Safe m12 m10, PolyApplicative m10 m11 m6,
      PolyApplicative m6 m8 m9, PolyApplicative m4 m5 m1,
      PolyApplicative m1 m2 m3,
      args~(m7 (m12 a), (m5 (m11 a), (m2 (m8 a), ()))),
      out~m3 (m9 (Vector a))) =>
    Func2 Cons_Vector args out where
    getFunc2 _ _ = cons_Vector_func2


call_s2 (AppH(fptr, args)) = (getFunc2 fptr args') args' where
    args' = readArgs args

call1 (AppH(fptr, args)) = tuplePipe (uncurryTuple $ getFunc fptr args') args' where
    args' = readArgs args

call2 (AppH(fptr, args)) = tuplePipe (uncurryTuple $ getFunc2 fptr args') args' where
    args' = readArgs args



call2D (AppH(fptr, args)) = tuplePipe (uncurryTuple $ getFuncD fptr) args' where
    args' = readArgs args

call3D (AppH(fptr, args)) = tupleApp2 (uncurryTuple $ getFuncD fptr) args' where
    args' = readArgs args

call4D (AppH(fptr, args)) = tupleApp3 (uncurryTuple $ getFuncD fptr) args' where
    args' = readArgs args

--call3Dxxx xxx (AppH(fptr, args)) = tupleApp2 (uncurryTuple $ getFuncD xxx) args' where
--    args' = readArgs args

call2D' (AppH(fptr, args)) = tuplePipe (uncurryTuple $ getFuncD2 fptr args') args' where
    args' = readArgs args



testme (AppH(fptr, args)) = getFunc2 fptr args' where
    args' = readArgs args

testmeD (AppH(fptr, args)) = getFuncD fptr 

testmeD2 (AppH(fptr, args)) = getFuncD fptr args' where
    args' = readArgs args

testmeArgs (AppH(fptr, args)) = args' where
    args' = readArgs args

---

testme2 (AppH(fptr, args)) = getFunc2 fptr (matchArgs args') where
    args' = readArgs args


--moze wywalic stad juz tuple pipe ? margs powinno robic to samo?
--albo uzyjemy dysfunctionali?
--call3 (AppH(fptr, args)) = tuplePipe (uncurryTuple $ getFunc2 fptr $ margs) $ margs where
--    args' = readArgs args
--    margs = matchArgs args'

--call3 (AppH(fptr, args)) = (getFunc2 fptr $ margs) $ margs where
--    args' = readArgs args
--    margs = matchArgs args'


class MatchArg arg out where
    matchArg :: arg -> out

instance MatchArg a b <= a~b where
    matchArg = id

instance MatchArg (MonadCtx base1 set1 m1 val1) (Req req (MonadCtx base2 set2 m2) val2) <= (base1~base2, set1~set2, m1~m2, val1~val2) where
    matchArg = Req

class MatchArgs args out where
    matchArgs :: args -> out


instance MatchArgs () () where
    matchArgs = id

instance MatchArgs (x,xs) (xout, xsout) <= (MatchArg x xout, MatchArgs xs xsout) where
    matchArgs (x,xs) = (matchArg x, matchArgs xs)




--class ApplyFTuple f args out | f args -> out where
--    applyFTuple :: f -> args -> out

--instance ApplyFTuple f () f where
--    applyFTuple = const

--instance ApplyFTuple (x -> f) (x,xs) f <= ApplyFTuple f xs f where
--    applyFTuple f (x,xs) = applyFTuple (f x) xs


class UncurryTuple f out | f -> out where
    uncurryTuple :: f -> out

instance UncurryTuple (() -> a) a where
    uncurryTuple f = f ()

instance UncurryTuple ((x,xs) -> f) (x -> fout) <= UncurryTuple (xs -> f) fout where
    uncurryTuple f = (\x -> uncurryTuple $ f . (x,))


--fxxx a = cons_Vector_func2 `pipe3` a

typeArg :: (a -> b) -> a -> (a -> b)
typeArg = const

--xxx args = cons_Vector_func2 args

--instance Func Cons_Vector2 args out <= (args~(m1 (s1 a), (m2 (s2 a), (m3 (s3 a), ()))), out~mout(sout(Vector a))) where
--    getFunc _ (a,(b,(c,()))) = cons_Vector_func2' `pipe2` a `pipe2` b `pipe2` c

--xxx (a,(b,(c,()))) = cons_Vector_func2' `pipe2` a `pipe2` b `pipe2` c
--xxx (a,()) = cons_VVV `pipe2` a

    --xxf f (a,(b,(c,()))) = f `pipe2` a `pipe2` b `pipe2` c

    --xxx = xxf cons_Vector_func2

--instance Func Cons_Vector2 args out where
--    getFunc _ args = cons_Vector_func `tuplePipe` args
--tstx args = cons_Vector_func `tuplePipe` args

--tstx (a,(b,(c,()))) = cons_Vector_func2 >>> a >>> b >>> c

    --tstx1 (a,(b,(c,()))) = cons_Vector_func2 `pipe2` a `pipe2` b `pipe2` c

--tstx2 (a :: (a,(b,(c,())))) = cons_Vector_func2 `tuplePipe` a

--tstx a = cons_VVV `tuplePipe` a

---
prop_Vector_bar (self,(a,(b,()))) = val (a, b)

data Prop_Vector_bar   = Prop_Vector_bar   deriving (Show, Eq, Typeable)

instance (args~(t, (t1, (t2, ()))), out~Value Pure (Safe (t1, t2)))
    => Func Prop_Vector_bar args out where
    getFunc _ = prop_Vector_bar

instance (args~(t, (t1, (t2, ()))), out~Value Pure (Safe (t1, t2)))
    => Func2 Prop_Vector_bar args out where
    getFunc2 _ _ = prop_Vector_bar

instance Prop "bar" (m(s(Vector a))) (AppH Prop_Vector_bar (NParam "self", (NParam "a", (NParam "b", ())))) where
    prop _ obj = appH Prop_Vector_bar $ (mkArg :: NParam "self") // (mkArg :: NParam "a") // (mkArg :: NParam "b") // ()

prop' name obj = appByName (Proxy::Proxy "self") obj $ prop name obj
prop'2 name obj = appByName' (Proxy::Proxy "self") obj $ prop name obj

---
prop_Vector_foo (self,(a,(b,()))) = (liftF2 (+)) a b

data Prop_Vector_foo   = Prop_Vector_foo   deriving (Show, Eq, Typeable)

instance (PolyApplicative (Value Pure) m5 m1, PolyApplicative Safe m8 m4, PolyApplicative m4 m6 m7, PolyApplicative m1 m2 m3, Num b, args~(t, (m5 (m8 b), (m2 (m6 b), ()))), out~(m3 (m7 b)))
    => Func2 Prop_Vector_foo args out where
    getFunc2 _ _ = prop_Vector_foo

instance (PolyApplicative (Value Pure) m5 m1, PolyApplicative Safe m8 m4, PolyApplicative m4 m6 m7, PolyApplicative m1 m2 m3, Num b, args~(t, (m5 (m8 b), (m2 (m6 b), ()))), out~(args -> m3 (m7 b)))
    => FuncD Prop_Vector_foo out where
    getFuncD _ = prop_Vector_foo

instance Prop "foo" (m(s(Vector a))) (AppH Prop_Vector_foo (NParam "self", (NParam "a", (NDParam "b" (Value Pure (Safe Int)), ())))) where
    prop _ obj = appH Prop_Vector_foo $ (mkArg :: NParam "self") // (mkArg :: NParam "a") // (mkArg (val 0) :: NDParam "b" (Value Pure (Safe Int))) // ()

---
prop_Vector_baz (self,(a,())) = flip runStateTX' (val 0) a

data Prop_Vector_baz   = Prop_Vector_baz   deriving (Show, Eq, Typeable)
data Prop_Vector_baz' a = Prop_Vector_baz' a deriving (Show, Eq, Typeable, Functor)

instance (MatchMonadCloseProto(IsEmpty (Remove (Proxy StateT) set))(MonadCtx env (Remove (Proxy StateT) set) mb)t1,Num a,
         args~(t,(Req(Proxy StateT)(MonadCtx env set (StateT (Value Pure (Safe a)) mb))a1,())),
         out~t1 (a1, Value Pure (Safe a)))
    => Func2 Prop_Vector_baz args out where
    getFunc2 _ _ = prop_Vector_baz


instance (MatchMonadCloseProto (IsEmpty (Remove (Proxy StateT) set0)) (MonadCtx env0 (Remove (Proxy StateT) set0) mb0) t1, Num a0,
         out~((t0,(Req(Proxy StateT)(MonadCtx env0 set0 (StateT (Value Pure (Safe a0)) mb0))a10,())) -> t1 (a10, Value Pure (Safe a0))))
    => FuncD Prop_Vector_baz out where
    getFuncD _ = prop_Vector_baz

instance (MatchMonadCloseProto (IsEmpty (Remove (Proxy StateT) set0)) (MonadCtx env0 (Remove (Proxy StateT) set0) mb0) t1, Num a0,
         out~((t0,(Req(Proxy StateT)(MonadCtx env0 set0 (StateT (Value Pure (Safe a0)) mb0))a10,())) -> t1 (a10, Value Pure (Safe a0))))
    => FuncD (Prop_Vector_baz' a) out where
    getFuncD _ = prop_Vector_baz

instance (MatchMonadCloseProto(IsEmpty (Remove (Proxy StateT) set))(MonadCtx env (Remove (Proxy StateT) set) mb)t1,Num a,
         args~(t,(Req(Proxy StateT)(MonadCtx env set (StateT (Value Pure (Safe a)) mb))a1,())),
         out~(args -> t1 (a1, Value Pure (Safe a))))
    => FuncD2 Prop_Vector_baz args out where
    getFuncD2 _ _ = prop_Vector_baz

instance Prop "baz" (m(s(Vector a))) (AppH Prop_Vector_baz (NParam "self", (NParam "a", ()))) where
    prop _ obj = appH Prop_Vector_baz $ (mkArg :: NParam "self") // (mkArg :: NParam "a") // ()

instance Prop "baz2" (m(s(Vector a))) (Prop_Vector_baz' (NParam "self", (NParam "a", ()))) where
    prop _ obj = Prop_Vector_baz' $ (mkArg :: NParam "self") // (mkArg :: NParam "a") // ()

---

prop_Vector_test (a,()) = (flip (runStateTX' . appMonadCtx2) (val 0)) a


data Prop_Vector_test   = Prop_Vector_test   deriving (Show, Eq, Typeable)

instance Prop "test" (m(s(Vector a))) (AppH Prop_Vector_test (NParam "a", ())) where
    prop _ obj = appH Prop_Vector_test $ (mkArg :: NParam "a") // ()


--instance ((MatchMonadCloseProto (IsEmpty (Remove (Proxy StateT) set0)) (MonadCtx env0 (Remove (Proxy StateT) set0) mb0) t0), Num a0,
--         out ~ ((Req (Proxy StateT) (MonadCtx env0 set0 (StateT (Value Pure (Safe a0)) mb0)) a10, ()) -> t0 (a10, Value Pure (Safe a0))))
--    => FuncD Prop_Vector_test out where
--    getFuncD _ = prop_Vector_test

instance (AppMonadCtx2 a (Req (Proxy StateT) (MonadCtx env set (StateT (Value Pure (Safe a2)) mb)) a1),
          MatchMonadCloseProto (IsEmpty (Remove (Proxy StateT) set)) (MonadCtx env (Remove (Proxy StateT) set) mb) t,
          Num a2,
          args ~ (a,()),
          out ~  t (a1, Value Pure (Safe a2))) 
          => Func2 Prop_Vector_test args out where
    getFunc2 _ _ = prop_Vector_test


---


prop_Vector_id (a,()) = a


data Prop_Vector_id   = Prop_Vector_id   deriving (Show, Eq, Typeable)

instance Prop "id" (m(s(Vector a))) (AppH Prop_Vector_id (NParam "a", ())) where
    prop _ obj = appH Prop_Vector_id $ (mkArg :: NParam "a") // ()


instance (out ~ ((t, ()) -> t)) => FuncD Prop_Vector_id out where
    getFuncD _ = prop_Vector_id

---

print' :: a -> MonadCtx IO () m () <= (MonadIO m, Show a)
print' s = MonadCtx . liftIO $ print s


--v ::Int
v = call2 $ appNext (val 1) $ appNext (val 2) $ appNext (val 3) cons_Vector


--argsRaw = appNext (Req getX) $ prop' (Proxy::Proxy "baz") v
--args = snd . fromAppH $ argsRaw
--args' = readArgs args


--args2 = snd . fromAppH $ appNext (getX) $ prop' (Proxy::Proxy "baz") v
--args2' = readArgs args2


argsRaw = appNext (Req getX) $ prop (Proxy::Proxy "test") v
args = snd . fromAppH $ argsRaw
args' = readArgs args


args2Raw = appNext (getX) $ prop (Proxy::Proxy "test") v
args2 = snd . fromAppH $ args2Raw
args2' = readArgs args2


bazArgsRaw = appNext (Req getX) $ appNext (val (0::Int)) $ prop (Proxy::Proxy "baz") v
bazArgs = snd . fromAppH $ bazArgsRaw
bazArgs' = readArgs bazArgs


bazArgs2Raw = appNext (getX) $ appNext (val (0::Int)) $ prop (Proxy::Proxy "baz") v
bazArgs2 = snd . fromAppH $ bazArgs2Raw
bazArgs2' = readArgs bazArgs2


idArgsRaw = appNext (val (0::Int)) $ prop (Proxy::Proxy "id") v
idArgs = snd . fromAppH $ idArgsRaw
idArgs' = readArgs idArgs


idArgs2Raw = appNext (getX) $ prop (Proxy::Proxy "id") v
idArgs2 = snd . fromAppH $ idArgs2Raw
idArgs2' = readArgs idArgs2

main' = do
    v2 <- call2 $ appNext (val 1) $ appNext (val 2) $ appNext (val 3) cons_Vector

    --v <- call $ appNext (val 1) $ appNext (val 2) $ appNext (val 3) cons_Vector
    print' v2
    print' $ call2 $ appNext (val 1) $ appNext (val 2) $ prop' (Proxy::Proxy "foo") v2
    print' $ call2 $ appByName (Proxy::Proxy "b") (val 1) $ appNext (val 2) $ prop' (Proxy::Proxy "foo") v2

    print' $ call2 $ appNext (val 2) $ appNext (val 1) $ prop' (Proxy::Proxy "bar") v2
    print' "end"


            --sygnatura getFunc (w call2 oraz testme) jest niepoprawna! Argumentem jest MonadCtx, natomiast funkcja oczekuje Req [...]
            --moze warto wrocic do MonadCtx zawierajacego Req by typy byly jednolite?
            --Ale to nie pomoze przy funkcjach w ktorych uzywa sie np. Value w miejscu MonadCtx!
            --print' $ testme $ appNext getX $ prop' (Proxy::Proxy "baz") v2


    --print' $ testme2 $ appNext getX $ prop' (Proxy::Proxy "baz") v2

    --print' $ call2D $ appNext (val 1) $ appNext (val 2) $ prop' (Proxy::Proxy "foo") v2
    --print' $ call2D $ appNext (getX) $ prop' (Proxy::Proxy "baz") v


    --print' $ tupleApp2 (uncurryTuple $ getFuncD Prop_Vector_baz) args2'

    --print' $ call3Dxxx $ appNext (getX) $ prop' (Proxy::Proxy "baz") v

    --call3Dxxx (AppH(fptr, args)) xxx = tupleApp2 (uncurryTuple $ getFuncD xxx) args' where
    --args' = readArgs args

    print' $ call_s2 $ appNext (getX) $ prop (Proxy :: Proxy "test") v2
    print' $ call_s2 $ appNext (val 5) $ prop (Proxy :: Proxy "test") v2
----main = print $ call $ appNext (val 1) $ appNext (val 2) $ appNext (val 3) cons_Vector
----main' = print' "hello"

call3Dxxx (AppH(fptr, args)) xxx = tupleApp2 (uncurryTuple $ getFuncD xxx) args' where
    args' = readArgs args

--call3Dxxx apph = tupleApp2 (uncurryTuple $ getFuncD Prop_Vector_baz) args' where
--    args = snd . fromAppH $ apph
--    args' = readArgs args

main = fromValue $ closeMonadCtx main'


----call2 (AppH(fptr, args)) = getFunc fptr `tuplePipe` args' where
----    args' = readArgs args




-- :t tupleApp2 (uncurryTuple $ getFuncD Prop_Vector_baz) args2'