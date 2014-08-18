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
{-# LANGUAGE TemplateHaskell #-}

--{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}

!{-# LANGUAGE RightSideContexts #-}
{-# LANGUAGE DysfunctionalDependencies #-}
{-# LANGUAGE DeriveGeneric #-}


{-# LANGUAGE RebindableSyntax #-}

{-# LANGUAGE AllowAmbiguousTypes #-}


module Main where

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
import Control.Monad.Morph
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

import Luna.Target.HS.TH.TH

--import VectorData

import qualified Prelude as P

import           Prelude hiding ((>>=),(>>), return, fail)
import Luna.Target.HS.Control.Context.Bind
import GHC.Generics

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
data Vector a = Vector a a a deriving (Show, Eq, Typeable)
data Cls_Vector = Cls_Vector deriving (Show, Eq, Typeable)


$(generateFieldAccessors ''Vector [Just "x", Just "y", Just "z"])

propSig_Cls_Vector_Vector = (mkArg :: NParam "self") // (mkArg (val (0::Int)) ~:: (u :: NDParam "x" a)) // (mkArg (val 0) :: NDParam "y" (Value Pure(Safe Int))) // (mkArg (val 0) :: NDParam "z" (Value Pure(Safe Int))) // ()
propDef_Cls_Vector_Vector = liftCons3 Vector
registerMethod ''Cls_Vector "Vector"


--instance (PolyApplicative (Value Pure) m7 m4,
--      PolyApplicative Safe m12 m10, PolyApplicative m10 m11 m6,
--      PolyApplicative m6 m8 m9, PolyApplicative m4 m5 m1,
--      PolyApplicative m1 m2 m3,
--      args~(self,(m7 (m12 a), (m5 (m11 a), (m2 (m8 a), ())))),
--      out~m3 (m9 (Vector a))) =>
--    Func Cls_Vector "Vector" args out where
--    getFunc _ _ = propDef_Cls_Vector_Vector


--instance HasProp "Vector" Cls_Vector (NParam "self", (NDParam "x" (Value Pure (Safe Int)), (NDParam "y" (Value Pure (Safe Int)), (NDParam "z" (Value Pure (Safe Int)), ())))) where
--    propSig _ = propSig_Cls_Vector_Vector


-----
propSig_Vector_x = (mkArg :: NParam "self") // ()
propDef_Vector_x (self,()) = liftF1 fieldGetter_Vector_Vector_x self
registerMethod ''Vector "x"

--instance ( PolyApplicative (Value Pure) m2 m3, PolyApplicative Safe m1 m4,
--           args~(m2 (m1 (Vector b)), ()), out~m3 (m4 b))
--    => Func Vector "x" args out where
--    getFunc _ _ = propDef_Vector_x

--instance HasProp "x" Vector (NParam "self", ()) where
--    propSig _ = propSig_Vector_x


-----
propSig_Vector_x_setter = (mkArg :: NParam "self") // (mkArg :: NParam "x") // ()
propDef_Vector_x_setter (self,(a,())) = liftF2 fieldSetter_Vector_Vector_x a self
registerMethod ''Vector "x_setter"

--instance ( PolyApplicative (Value Pure) m5 m1, PolyApplicative Safe m8 m4,
--           PolyApplicative m4 m6 m7, PolyApplicative m1 m2 m3,
--           args~(m2 (m6 (Vector a1)), (m5 (m8 a1), ())), out~m3 (m7 (Vector a1)))
--    => Func Vector "x_setter" args out where
--    getFunc _ _ = propDef_Vector_x_setter

--instance HasProp "x_setter" Vector (NParam "self", (NParam "x", ())) where
--    propSig _ = propSig_Vector_x_setter


---
propSig_Vector_bar = (mkArg :: NParam "self") // (mkArg :: NParam "a") // (mkArg :: NParam "b") // ()
propDef_Vector_bar (self,(a,(b,()))) = val (a, b)
registerMethod ''Vector "bar"

--instance (args~(t, (t1, (t2, ()))), out~Value Pure (Safe (t1, t2)))
--    => Func Vector "bar" args out where
--    getFunc _ _ = propDef_Vector_bar

--instance HasProp "bar" Vector (NParam "self", (NParam "a", (NParam "b", ()))) where
--    propSig _ = propSig_Vector_bar


---
propSig_Vector_foo = (mkArg :: NParam "self") // (mkArg :: NParam "a") // (mkArg (val 0) :: NDParam "b" (Value Pure (Safe Int))) // ()
propDef_Vector_foo (self,(a,(b,()))) = (liftF2 (+)) a b
registerMethod ''Vector "foo"

--instance (PolyApplicative (Value Pure) m5 m1, PolyApplicative Safe m8 m4, PolyApplicative m4 m6 m7, PolyApplicative m1 m2 m3, Num b, args~(t, (m5 (m8 b), (m2 (m6 b), ()))), out~(m3 (m7 b)))
--    => Func Vector "foo" args out where
--    getFunc _ _ = propDef_Vector_foo

--instance HasProp "foo" Vector (NParam "self", (NParam "a", (NDParam "b" (Value Pure (Safe Int)), ()))) where
--    propSig _ = propSig_Vector_foo


---
propSig_Vector_baz = (mkArg :: NParam "self") // (mkArg :: NParam "a") // ()
propDef_Vector_baz (self,(a,())) = flip runStateTX (val 0) a
registerMethod ''Vector "baz"

--instance (AppMonadCtx a (Req (Proxy StateT) (MonadCtx env set (StateT (Value Pure (Safe a2)) mb)) a1), Functor mb,
--          MatchMonadCloseProto (IsEmpty (Remove (Proxy StateT) set)) (MonadCtx env (Remove (Proxy StateT) set) mb) t1, Num a2,
--          args ~ (t, (a, ())),
--          out ~ t1 (Safe (a1, Value Pure (Safe a2))))
--      => Func Vector "baz" args out where
--    getFunc _ _ = propDef_Vector_baz

--instance HasProp "baz" Vector (NParam "self", (NParam "a", ())) where
--    propSig _ = propSig_Vector_baz


---
propSig_Vector_fstate = (mkArg :: NParam "self") // ()
propDef_Vector_fstate (self,()) = do
    x <- getX
    putX (x)

registerMethod ''Vector "fstate"

--instance (MonadState (Value Pure (Safe a)) m, Num a,
--         args ~ (t, ()),
--         out ~ MonadCtx Pure (Proxy StateT, ()) m (Value Pure (Safe ())))
--      => Func Vector "fstate" args out where
--    getFunc _ _ = propDef_Vector_fstate

--instance HasProp "fstate" Vector (NParam "self", ()) where
--    propSig _ = propSig_Vector_fstate

---


print' :: a -> MonadCtx IO () m (Value Pure (Safe ())) <= (MonadIO m, Show a)
print' s = MonadCtx . liftIO $ fmap val $ print s


print2 = autoLift1 print

tst = do
    val (1::Int)
    --print2 $ val "ala"
    val (2::Int)


data Err1 = Err1 deriving (Show)
data Err2 = Err2 deriving (Show)


tstErr = do
    x <- raise Err1 $ val 5
    val 6
    catch (\Err2 -> val 0) x





v = call $ appNext (val 2) $ appNext (val 2) $ appNext (val 2) $ objProp (Proxy::Proxy "Vector") (val Cls_Vector)
a1 = call $ appNext (val 1) $ appNext (val 2) $ objProp (Proxy::Proxy "foo") v


main' = do
    v2 <- call $ appByName (Proxy :: Proxy "z") (val 3) $ appNext (val 2) $ appNext (val 1) $ objProp (Proxy::Proxy "Vector") (val Cls_Vector)
    print2 v2

    print2 $ call $ appNext (val 1) $ appNext (val 2) $ objProp (Proxy::Proxy "foo") v2
    print2 $ call $ appByName (Proxy::Proxy "b") (val 1) $ appNext (val 2) $ objProp (Proxy::Proxy "foo") v2

    print2 $ call $ appNext (val 2) $ appNext (val 1) $ objProp (Proxy::Proxy "bar") v2
    print2 (val "---")


    print2 $ call $ appNext (getX) $ objProp (Proxy :: Proxy "baz") v2
    print2 $ call $ appNext (val 5) $ objProp (Proxy :: Proxy "baz") v2


    print2 $ flip runStateTX (val 0) $ call $ objProp (Proxy :: Proxy "fstate") v2

    -- properties
    print2 $ call $ objProp (Proxy::Proxy "x") v2
    print2 $ call $ appNext (val 5) $ objProp (Proxy::Proxy "x_setter") v2

--    --print'2 tst
    print2 $ val "end"
    print2 tstErr
    print2 $ val "end2"


main = fromValue $ main'



