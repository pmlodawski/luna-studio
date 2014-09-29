{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DysfunctionalDependencies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- module --
module Main3 where

-- imports --
import Luna.Target.HS

import qualified Prelude
import Control.PolyApplicative
import Control.PolyApplicative.App
import Control.PolyMonad
import Data.TupleList


ofType :: a -> a -> a
ofType = const

u = undefined

p = Proxy
type P = Proxy

(~::) = ofType


--print2 = autoLift1 print

print_DBG val = Value $ fmap Safe $ print val




print2 = polyJoin . liftF1 (Value . fmap Safe . print)


oldtest = get5X >>= id

-- monad tests - should compile.
tstme x = x >>= (\_ -> val 0)
tstme2 = (val 1) >>= (\_ -> val (0::Int))
tstme3 = (val (1::Int)) >>= (\_ -> val 0)
tstme4 = (val 1) >>= (\_ -> val 0)
tstme5 x y = x >>= (\_ -> y >>= (\_ -> val 0))
tstme6 = get5X >>= id


--nw = runStateT3X (liftF2' (+) get5X  (val 2)) (val 0)

-------------------------------------------------------------------------------

--class Vector a:
--    x,y,z :: a = 0
--    def foo a b=0: a+b
--    def bar a b: (a,b)
--    def baz x: runState @x
--    def fstate:
--        x = get
--        put (x+1)

data Cls_ModuleVector = Cls_ModuleVector deriving (Show, Eq, Typeable)
data ModuleVector = ModuleVector deriving (Show, Eq, Typeable)
memSig_Cls_ModuleVector_ModuleVector = (mkArg :: NParam "self") // ()
memDef_Cls_ModuleVector_ModuleVector (self, ()) = val ModuleVector --liftCons0 ModuleVector
registerMethod ''Cls_ModuleVector "ModuleVector"

--cons_ModuleVector = member (Proxy::Proxy "ModuleVector") (val Cls_ModuleVector)

cons_ModuleVector = member (Proxy::Proxy "ModuleVector") (val Cls_ModuleVector)



---
data Cls_Vector = Cls_Vector deriving (Show, Eq, Typeable)
data Vector a = Vector a a a deriving (Show, Eq, Typeable)
generateFieldAccessors 'Vector [Just "x", Just "y", Just "z"]

memSig_Cls_Vector_Vector = (mkArg :: NParam "self") // (mkArg (val (0::Int)) ~:: (u :: NDParam "x" a)) // (mkArg (val 0) :: NDParam "y" (Value Pure Safe Int)) // (mkArg (val 0) :: NDParam "z" (Value Pure Safe Int)) // ()
memDef_Cls_Vector_Vector = liftCons3 Vector


--memDef_Cls_Vector_Vector2 (self, (x, (y, (z, ())))) = polyJoin $ val (\a b c -> val Vector `appBindCtx` a `appBindCtx` b `appBindCtx` c) <<*>> x <<*>> y <<*>> z

registerMethod ''Cls_Vector "Vector"

cons_Vector = member (Proxy::Proxy "Vector") (val Cls_Vector)




-----
memSig_Vector_x = (mkArg :: NParam "self") // ()
memDef_Vector_x (self,()) = liftF1 fieldGetter_Vector_Vector_x self
registerMethod ''Vector "x"

-----
memSig_Vector_x_setter = (mkArg :: NParam "self") // (mkArg :: NParam "x") // ()
memDef_Vector_x_setter (self,(a,())) = liftF2 fieldSetter_Vector_Vector_x a self
registerMethod ''Vector "x_setter"

---
memSig_Vector_bar = (mkArg :: NParam "self") // (mkArg :: NParam "a") // (mkArg :: NParam "b") // ()
memDef_Vector_bar (self,(a,(b,()))) = val (a, b)
registerMethod ''Vector "bar"

---
memSig_Vector_foo = (mkArg :: NParam "self") // (mkArg :: NParam "a") // (mkArg (val 0) :: NDParam "b" (Value Pure Safe Int)) // ()
memDef_Vector_foo (self,(a,(b,()))) = liftF2 (+) a b
registerMethod ''Vector "foo"

---
memSig_Vector_baz = (mkArg :: NParam "self") // (mkArg :: NParam "a") // ()
memDef_Vector_baz (self,(a,())) = flip runStateT3X (val 0) a
registerMethod ''Vector "baz"

---

tst = do
    x <- get5X
    put5X x
    val 6


tst2 = do
    t1' <- get5X
    t1'


tst4 = do
    t1' <- get5X

    t1'

--id :: a -> a

--get5X :: MonadState4 val m Safe => MonadCtx2 (Value Pure) (Insert (Proxy StateT3) Empty) m Safe val

--class PolyMonad5 m1 m2 m3 | m1 m2 -> m3 where
--    polyBind5 :: m1 a -> (X1 m1 a -> m2 c) -> m3 c

--type family X1 m a where
--    X1 (MonadCtx2 env1 set1 m1 s1)          a = a
--    X1 (Value m s)                         a = (Value Pure s a)
--    X1 (MonadCtx2Dummy env set m2 s2 m1 s1) a = (Value m2 s2 a)


--polyBind5 :: MonadCtx2 (Value Pure) (Insert (Proxy StateT3) Empty) m Safe (m2 c) -> (m2 c -> m2 c) -> m3 c <= MonadState4 (m2 c) m Safe


--tst3 = get5X `polyBind5` id - czemu to sie nie kompiluje?
--tst3 = (val 0) `polyBind5` id - a to tak
--i analogicznie: runMonad2 ... $ liftF2' (+) get5X (val 0)
--tst3 = get5X `polyCtxBind` id - to tez sie nie kompiluje (tam jest prim). Co oznacza ze type family jest ok. 

--class PolyMonad5 m1 m2 m3 | m1 m2 -> m3 where
--    polyBind5 :: m1 a -> (X1 m1 a -> m2 c) -> m3 c

--    polyBind5 :: m1 a -> (m2 c -> m2 c) -> m3 c

--polyBind5 :: m1 (m2 c) -> (m2 c -> m2 c) -> m3 c



    --let x :: Int
    --    x = t1'
    ----t2' <- val 6
    --val 0
    --val (+) <<*>> t1' <<*>> t2'

---

memSig_Vector_fstate = (mkArg :: NParam "self") // ()
memDef_Vector_fstate (self,()) = do
    x <- get5X
    put5X x

registerMethod ''Vector "fstate"

    --xxx = do
    --    x <- getX
    --    putX x

    ----instance (MonadState (Value Pure (Safe a)) m, Num a,
    ----         args ~ (t, ()),
    ----         out ~ MonadCtx Pure (Proxy StateT, ()) m (Value Pure (Safe ())))
    ----      => Func Vector "fstate" args out where
    ----    getFunc _ _ = memDef_Vector_fstate

    ----instance HasProp "fstate" Vector (NParam "self", ()) where
    ----    memSig _ = memSig_Vector_fstate

    -----


    ----print' :: a -> MonadCtx IO () m (Value Pure (Safe ())) <= (MonadIO m, Show a)
    ----print' s = MonadCtx . liftIO $ fmap val $ print s




    --tst = do
    --    val (1::Int)
    --    --print2 $ val "ala"
    --    val (2::Int)


    --data Err1 = Err1 deriving (Show)
    --data Err2 = Err2 deriving (Show)


    --tstErr = do
    --    x <- raise Err1 $ val 5
    --    val 6
    --    catch (\Err2 -> val 0) x





v = call $ appByName (Proxy :: Proxy "z") (val 3) $ appNext (val 2) $ appNext (val 1) $ cons_Vector
    --a1 = call $ appNext (val 1) $ appNext (val 2) $ member (Proxy::Proxy "foo") v


    --mkLam lam sig = val $ appH (Lam lam) sig

mymain (self,()) = do
--    --mod <- call $ member (Proxy::Proxy "ModuleVector") (val Cls_ModuleVector)
--    --call $ member (Proxy::Proxy "main") mod


    v2 <- call $ appByName (Proxy :: Proxy "z") (val 3) $ appNext (val 2) $ appNext (val 1) $ member (Proxy::Proxy "Vector") (val Cls_Vector)

    let lam1 = mkLam (\x -> v2) ((mkArg :: NParam "self") // ())

    print_DBG $ call $ appNext (val 1) lam1

    print_DBG v2

    print_DBG $ call $ appNext (val 1) $ appNext (val 2) $ member (Proxy::Proxy "foo") v2
    print_DBG $ call $ appByName (Proxy::Proxy "b") (val 1) $ appNext (val 2) $ member (Proxy::Proxy "foo") v2

    print_DBG $ call $ appNext (val 2) $ appNext (val 1) $ member (Proxy::Proxy "bar") v2
    print_DBG (val "---")


    print_DBG $ call $ appNext (get5X) $ member (Proxy :: Proxy "baz") v2
    print_DBG $ call $ appNext (val 5) $ member (Proxy :: Proxy "baz") v2

    print_DBG $ flip runStateT3X (val 0) $ call $ member (Proxy :: Proxy "fstate") v2

    -- properties
    print_DBG $ call $ member (Proxy::Proxy "x") v2
    print_DBG $ call $ appNext (val 5) $ member (Proxy::Proxy "x_setter") v2

    val 5


memSig_ModuleVector_main = (mkArg :: NParam "self") // ()
memDef_ModuleVector_main = mymain
registerMethod ''ModuleVector "main"


main = mainMaker cons_ModuleVector




