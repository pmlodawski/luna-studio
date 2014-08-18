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
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

--{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}

!{-# LANGUAGE RightSideContexts #-}
{-# LANGUAGE DysfunctionalDependencies #-}


{-# LANGUAGE RebindableSyntax #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PackageImports #-}


module Vector where

import Control.PolyMonad
import Control.Monad (join)
import Control.Monad.Shuffle
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.PolyApplicative
import Control.PolyApplicative.App
import Luna.Target.HS.Control.Context
import Luna.Target.HS.Control.Flow
import Luna.Target.HS.Utils.BaseMonads
import Luna.Target.HS.Data.Func
import "luna-target-ghchs" Control.Monad.Morph
import Flowbox.Utils
import Data.Typeable (Typeable, Proxy(..))
import Data.TypeLevel
import Data.Wrap
import Luna.Target.HS.Data.Struct
import Luna.Target.HS.Host.Lift
--import Luna.Target.HS.Control.Context.Rebindable
import GHC.TypeLits (Symbol)
import Data.TupleList
import Type.BaseType
--import Type.Infer
import Data.Proxy.Utils

--import VectorData

import qualified Prelude as P

import           Prelude hiding ((>>=),(>>), return, fail)
import Luna.Target.HS.Control.Context.Bind
(>>=)  = bindEnv
(>>)   = bindEnv_
fail _ = undefined
return = P.return

--bindEnv2 a f = bindEnv a (inferTypeBase . f)
--bindEnv2_ a b = bindEnv2 a (\_ -> b)

ofType :: a -> a -> a
ofType = const

u = undefined

p = Proxy
type P = Proxy

(~::) = ofType

-------------------------------------------------------------------------------

--class Vector a:
--    x,y,z :: a = 0
--    def foo a b=0: a+b
--    def bar a b: (a,b)
--    def baz x: runState @x
--    def fstate:
--        x = get
--        put (x+1)

---
data Vector a = Vector { _prop_Vector_x :: a
                       , _prop_Vector_y :: a
                       , _prop_Vector_z :: a
                       } deriving (Show, Eq, Typeable)



cons_Vector_func (self,(x,(y,(z,())))) = liftF3 Vector x y z

instance (PolyApplicative (Value Pure) m7 m4,
      PolyApplicative Safe m12 m10, PolyApplicative m10 m11 m6,
      PolyApplicative m6 m8 m9, PolyApplicative m4 m5 m1,
      PolyApplicative m1 m2 m3,
      args~(self,(m7 (m12 a), (m5 (m11 a), (m2 (m8 a), ())))),
      out~m3 (m9 (Vector a))) =>
    Func (Prop (Meta Vector) "constructor") args out where
    getFunc _ _ = cons_Vector_func

instance HasProp "constructor" (Meta Vector) (NParam "self", (NDParam "x" (Value Pure (Safe Int)), (NDParam "y" (Value Pure (Safe Int)), (NDParam "z" (Value Pure (Safe Int)), ())))) where
    propSig _ = (mkArg :: NParam "self") // (mkArg (val (0::Int)) ~:: (u :: NDParam "x" a)) // (mkArg (val 0) :: NDParam "y" (Value Pure(Safe Int))) // (mkArg (val 0) :: NDParam "z" (Value Pure(Safe Int))) // ()

---
prop_Vector_bar (self,(a,(b,()))) = val (a, b)

instance (args~(t, (t1, (t2, ()))), out~Value Pure (Safe (t1, t2)))
    => Func (Prop Vector "bar") args out where
    getFunc _ _ = prop_Vector_bar

instance HasProp "bar" Vector (NParam "self", (NParam "a", (NParam "b", ()))) where
    propSig _ = (mkArg :: NParam "self") // (mkArg :: NParam "a") // (mkArg :: NParam "b") // ()


---
prop_Vector_foo (self,(a,(b,()))) = (liftF2 (+)) a b

instance (PolyApplicative (Value Pure) m5 m1, PolyApplicative Safe m8 m4, PolyApplicative m4 m6 m7, PolyApplicative m1 m2 m3, Num b, args~(t, (m5 (m8 b), (m2 (m6 b), ()))), out~(m3 (m7 b)))
    => Func (Prop Vector "foo") args out where
    getFunc _ _ = prop_Vector_foo

instance HasProp "foo" Vector (NParam "self", (NParam "a", (NDParam "b" (Value Pure (Safe Int)), ()))) where
    propSig _ = (mkArg :: NParam "self") // (mkArg :: NParam "a") // (mkArg (val 0) :: NDParam "b" (Value Pure (Safe Int))) // ()

---
prop_Vector_baz (self,(a,())) = flip runStateTX (val 0) a

instance (AppMonadCtx a (Req (Proxy StateT) (MonadCtx env set (StateT (Value Pure (Safe a2)) mb)) a1), Functor mb,
          MatchMonadCloseProto (IsEmpty (Remove (Proxy StateT) set)) (MonadCtx env (Remove (Proxy StateT) set) mb) t1, Num a2,
          args ~ (t, (a, ())),
          out ~ t1 (Safe (a1, Value Pure (Safe a2))))
      => Func (Prop Vector "baz") args out where
    getFunc _ _ = prop_Vector_baz

instance HasProp "baz" Vector (NParam "self", (NParam "a", ())) where
    propSig _ = (mkArg :: NParam "self") // (mkArg :: NParam "a") // ()

            -----

prop_Vector_test (a,()) = (flip runStateTX (val 0)) a

instance HasProp "test" Vector (NParam "a", ()) where
    propSig _ = (mkArg :: NParam "a") // ()

instance (AppMonadCtx a (Req (Proxy StateT) (MonadCtx env set (StateT (Value Pure (Safe a2)) mb)) a1), Functor mb,
          MatchMonadCloseProto (IsEmpty (Remove (Proxy StateT) set)) (MonadCtx env (Remove (Proxy StateT) set) mb) t, Num a2,
          args ~ (a,()),
          out ~  t (Safe (a1, Value Pure (Safe a2))))
          => Func (Prop Vector "test") args out where
    getFunc _ _ = prop_Vector_test

---
--prop_Vector_fstate :: (MonadState (Value Pure (Safe a)) m, Num a) =>
--                      (t, ()) -> MonadCtx Pure (Proxy StateT, ()) m ()
prop_Vector_fstate (self,()) = do
    x <- getX
    putX (x)

--prop_Vector_fstate (self,()) = do
--    x <- getX
--    putX (fromPure $ fromValue x)
    --val 5

instance (MonadState (Value Pure (Safe a)) m, Num a,
         args ~ (t, ()),
         out ~ MonadCtx Pure (Proxy StateT, ()) m (Value Pure (Safe ())))
      => Func (Prop Vector "fstate") args out where
    getFunc _ _ = prop_Vector_fstate

instance HasProp "fstate" Vector (NParam "self", ()) where
    propSig _ = (mkArg :: NParam "self") // ()

---

    --prop_Vector_id (a,()) = a


    --data Prop_Vector_id   = Prop_Vector_id   deriving (Show, Eq, Typeable)

    --instance HasProp "id" (m(s(Vector a))) (AppH Prop_Vector_id (NParam "a", ())) where
    --    getProp _ obj = appH Prop_Vector_id $ (mkArg :: NParam "a") // ()

    --class Test a b | a -> b where
    --    test :: a -> b

    --instance Test (m(s(Vector a))) Int where
    --    test _ = 5

---

print' :: a -> MonadCtx IO () m (Value Pure (Safe ())) <= (MonadIO m, Show a)
print' s = MonadCtx . liftIO $ fmap val $ print s


print2 = autoLift1 print

--tst = do
--    val (1::Int)
--    --print' "ala"
--    val (2::Int)


data Err1 = Err1 deriving (Show)
data Err2 = Err2 deriving (Show)


tstErr = do
    x <- raise Err1 $ val 5
    val 6
    catch (\Err2 -> val 0) x





v = call $ appNext (val 2) $ appNext (val 2) $ appNext (val 2) $ objProp (Proxy::Proxy "constructor") (val (meta :: Meta Vector))
a1 = call $ appNext (val 1) $ appNext (val 2) $ objProp (Proxy::Proxy "foo") v


main' = do
    v2 <- call $ appNext (val 2) $ appNext (val 2) $ appNext (val 2) $ objProp (Proxy::Proxy "constructor") (val (meta :: Meta Vector))

    print2 $ call $ appNext (val 1) $ appNext (val 2) $ objProp (Proxy::Proxy "foo") v2
    print2 $ call $ appByName (Proxy::Proxy "b") (val 1) $ appNext (val 2) $ objProp (Proxy::Proxy "foo") v2

    print2 $ call $ appNext (val 2) $ appNext (val 1) $ objProp (Proxy::Proxy "bar") v2
    print2 (val "---")


    print2 $ call $ appNext (getX) $ getProp (Proxy :: Proxy "test") v2
    print2 $ call $ appNext (val 5) $ getProp (Proxy :: Proxy "test") v2

    print2 $ call $ appNext (getX) $ objProp (Proxy :: Proxy "baz") v2
    print2 $ call $ appNext (val 5) $ objProp (Proxy :: Proxy "baz") v2


    print2 $ flip runStateTX (val 0) $ call $ objProp (Proxy :: Proxy "fstate") v2



    --print' $ shuffleJoin (val $ val 5)

--    --print'2 tst
    print2 $ val "end"
    print2 tstErr
    print2 $ val "end2"


main = fromValue $ main'



