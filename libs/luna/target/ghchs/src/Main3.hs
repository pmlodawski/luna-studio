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

-- module --
module Main where

-- imports --
import Luna.Target.HS hiding ((>>=), (>>), fail, return)

import qualified Prelude
import Control.PolyApplicative
import Control.PolyApplicative.App
import Control.PolyMonad
import Data.TupleList

--bindEnv2 a f = bindEnv a (inferTypeBase . f)
--bindEnv2_ a b = bindEnv2 a (\_ -> b)

ofType :: a -> a -> a
ofType = const

u = undefined

p = Proxy
type P = Proxy

(~::) = ofType


--print2 = autoLift1 print

print_DBG val = ValueS $ fmap Safe $ print val


(>>=)  = polyBind5'
(>>)   = polyBind5'_
fail _ = undefined
return = Prelude.return

class UnpackCtxS ctx m | ctx -> m where
    unpackCtxS :: ctx s a -> m (s a)

instance UnpackCtxS PureS Pure where
    unpackCtxS = fromPureS

instance UnpackCtxS IOS IO where
    unpackCtxS = fromIOS



class PackCtxS m ctx | m -> ctx where
    packCtxS :: m (s a) -> ctx s a

instance PackCtxS Pure PureS where
    packCtxS = PureS

instance PackCtxS IO IOS where
    packCtxS = IOS



liftF0' f = valS f
liftF1' f t1 = do
    t1' <- t1
    valS f <<*>> t1

liftF2' f t1 t2 = do
    t1' <- t1
    t2' <- t2
    valS f <<*>> t1 <<*>> t2


liftF3' f t1 t2 t3 = do
    t1' <- t1
    t2' <- t2
    t3' <- t3
    valS f <<*>> t1 <<*>> t2 <<*>> t3


liftF4' f t1 t2 t3 t4 = do
    t1' <- t1
    t2' <- t2
    t3' <- t3
    t4' <- t4
    valS f <<*>> t1 <<*>> t2 <<*>> t3 <<*>> t4

liftF5' f t1 t2 t3 t4 t5 = do
    t1' <- t1
    t2' <- t2
    t3' <- t3
    t4' <- t4
    t5' <- t5
    valS f <<*>> t1 <<*>> t2 <<*>> t3 <<*>> t4 <<*>> t5


liftCons0' = curryTuple1 . const . liftF0'
liftCons1' = curryTuple2 . const . liftF1'
liftCons2' = curryTuple3 . const . liftF2'
liftCons3' = curryTuple4 . const . liftF3'
liftCons4' = curryTuple5 . const . liftF4'
liftCons5' = curryTuple6 . const . liftF5'

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
memDef_Cls_ModuleVector_ModuleVector (self, ()) = valS ModuleVector --liftCons0 ModuleVector
registerMethod ''Cls_ModuleVector "ModuleVector"

cons_ModuleVector = member (Proxy::Proxy "ModuleVector") (val Cls_ModuleVector)

cons_ModuleVector2 = member2 (Proxy::Proxy "ModuleVector") (valS Cls_ModuleVector)



---
data Cls_Vector = Cls_Vector deriving (Show, Eq, Typeable)
data Vector a = Vector a a a deriving (Show, Eq, Typeable)
generateFieldAccessors 'Vector [Just "x", Just "y", Just "z"]

memSig_Cls_Vector_Vector = (mkArg :: NParam "self") // (mkArg (valS (0::Int)) ~:: (u :: NDParam "x" a)) // (mkArg (valS 0) :: NDParam "y" (ValueS Pure Safe Int)) // (mkArg (valS 0) :: NDParam "z" (ValueS Pure Safe Int)) // ()
memDef_Cls_Vector_Vector = liftCons3' Vector



memDef_Cls_Vector_Vector2 (self, (x, (y, (z, ())))) = polyJoin $ valS (\a b c -> valS Vector `appBind5` a `appBind5` b `appBind5` c) <<*>> x <<*>> y <<*>> z

registerMethod ''Cls_Vector "Vector"

cons_Vector = member2 (Proxy::Proxy "Vector") (valS Cls_Vector)



--liftF0' = ValueS . Pure . Safe


--liftF1' f a = do
--    a' <- a


--memDef_Cls_Vector_Vector2 x y z = do
--    Value2(PureS(Pure sx)) <- x
--    Value2(PureS(Pure sy)) <- y
--    Value2(PureS(Pure sz)) <- z
--    Value2 $ PureS $ Pure $ Safe Vector <<*>> sx <<*>> sy <<*>> sz




---

--class Functor2 ca ea cb eb f g | ca ea cb eb f -> g where
--    fmap2 :: (ca (ea a) -> cb (eb b)) -> f a -> g b

--data Cls_Vector2 = Cls_Vector2 deriving (Show, Eq, Typeable)
--data Vector2 c1 e1 c2 e2 c3 e3 a = Vector2 (c1 (e1 a)) (c2 (e2 a)) (c3 (e3 a)) deriving (Show, Eq, Typeable)
--generateFieldAccessors 'Vector2 [Just "x2", Just "y2", Just "z2"]

--memSig_Cls_Vector2_Vector2 = (mkArg :: NParam "self") // (mkArg (val (0::Int)) ~:: (u :: NDParam "x" a)) // (mkArg (val 0) :: NDParam "y" (Value Pure(Safe Int))) // (mkArg (val 0) :: NDParam "z" (Value Pure(Safe Int))) // ()
--memDef_Cls_Vector2_Vector2 (self,(x,(y,(z,())))) = val $ Vector2 x y z
--registerMethod ''Cls_Vector2 "Vector2"

--cons_Vector2 = member (Proxy::Proxy "Vector2") (val Cls_Vector2)


--instance Functor2 ca ea cb eb (Vector2 ca ea ca ea ca ea) (Vector2 cb eb cb eb cb eb) where
--    fmap2 f (Vector2 x y z) = Vector2 (f x) (f y) (f z)


--xxx2 :: (ca (ea a) -> cb (eb b)) -> Vector2 ca ea ca ea ca ea a -> Vector2 cb eb cb eb cb eb b
--xxx2 f (Vector2 x y z) = Vector2 (f x) (f y) (f z)

---







--instance (PolyApplicative (Value Pure) m7 m4,
--      PolyApplicative Safe m12 m10, PolyApplicative m10 m11 m6,
--      PolyApplicative m6 m8 m9, PolyApplicative m4 m5 m1,
--      PolyApplicative m1 m2 m3,
--      args~(self,(m7 (m12 a), (m5 (m11 a), (m2 (m8 a), ())))),
--      out~m3 (m9 (Vector a))) =>
--    Func Cls_Vector "Vector" args out where
--    getFunc _ _ = memDef_Cls_Vector_Vector


--instance HasProp "Vector" Cls_Vector (NParam "self", (NDParam "x" (Value Pure (Safe Int)), (NDParam "y" (Value Pure (Safe Int)), (NDParam "z" (Value Pure (Safe Int)), ())))) where
--    memSig _ = memSig_Cls_Vector_Vector


-----
memSig_Vector_x = (mkArg :: NParam "self") // ()
memDef_Vector_x (self,()) = liftF1' fieldGetter_Vector_Vector_x self
registerMethod ''Vector "x"

--instance ( PolyApplicative (Value Pure) m2 m3, PolyApplicative Safe m1 m4,
--           args~(m2 (m1 (Vector b)), ()), out~m3 (m4 b))
--    => Func Vector "x" args out where
--    getFunc _ _ = memDef_Vector_x

--instance HasProp "x" Vector (NParam "self", ()) where
--    memSig _ = memSig_Vector_x


-----
memSig_Vector_x_setter = (mkArg :: NParam "self") // (mkArg :: NParam "x") // ()
memDef_Vector_x_setter (self,(a,())) = liftF2' fieldSetter_Vector_Vector_x a self
registerMethod ''Vector "x_setter"

--instance ( PolyApplicative (Value Pure) m5 m1, PolyApplicative Safe m8 m4,
--           PolyApplicative m4 m6 m7, PolyApplicative m1 m2 m3,
--           args~(m2 (m6 (Vector a1)), (m5 (m8 a1), ())), out~m3 (m7 (Vector a1)))
--    => Func Vector "x_setter" args out where
--    getFunc _ _ = memDef_Vector_x_setter

--instance HasProp "x_setter" Vector (NParam "self", (NParam "x", ())) where
--    memSig _ = memSig_Vector_x_setter


---
memSig_Vector_bar = (mkArg :: NParam "self") // (mkArg :: NParam "a") // (mkArg :: NParam "b") // ()
memDef_Vector_bar (self,(a,(b,()))) = valS (a, b)
registerMethod ''Vector "bar"

--instance (args~(t, (t1, (t2, ()))), out~Value Pure (Safe (t1, t2)))
--    => Func Vector "bar" args out where
--    getFunc _ _ = memDef_Vector_bar

--instance HasProp "bar" Vector (NParam "self", (NParam "a", (NParam "b", ()))) where
--    memSig _ = memSig_Vector_bar


---
memSig_Vector_foo = (mkArg :: NParam "self") // (mkArg :: NParam "a") // (mkArg (val 0) :: NDParam "b" (Value Pure (Safe Int))) // ()
memDef_Vector_foo (self,(a,(b,()))) = liftF2' (+) a b
registerMethod ''Vector "foo"

--instance (PolyApplicative (Value Pure) m5 m1, PolyApplicative Safe m8 m4, PolyApplicative m4 m6 m7, PolyApplicative m1 m2 m3, Num b, args~(t, (m5 (m8 b), (m2 (m6 b), ()))), out~(m3 (m7 b)))
--    => Func Vector "foo" args out where
--    getFunc _ _ = memDef_Vector_foo

--instance HasProp "foo" Vector (NParam "self", (NParam "a", (NDParam "b" (Value Pure (Safe Int)), ()))) where
--    memSig _ = memSig_Vector_foo


---
memSig_Vector_baz = (mkArg :: NParam "self") // (mkArg :: NParam "a") // ()
memDef_Vector_baz (self,(a,())) = flip runStateT3X (valS 0) a
registerMethod ''Vector "baz"

--instance (AppMonadCtx a (Req (Proxy StateT) (MonadCtx env set (StateT (Value Pure (Safe a2)) mb)) a1), Functor mb,
--          MatchMonadCloseProto (IsEmpty (Remove (Proxy StateT) set)) (MonadCtx env (Remove (Proxy StateT) set) mb) t1, Num a2,
--          args ~ (t, (a, ())),
--          out ~ t1 (Safe (a1, Value Pure (Safe a2))))
--      => Func Vector "baz" args out where
--    getFunc _ _ = memDef_Vector_baz

--instance HasProp "baz" Vector (NParam "self", (NParam "a", ())) where
--    memSig _ = memSig_Vector_baz

testX1 :: m a -> b -> X1 m b
testX1 = undefined

tst = do
    x <- get5X
    put5X x
    valS 6


tst2 = do
    t1' <- valS 0
    t1'


--id :: a -> a

--get5X :: MonadState4 val m Safe => MonadCtx2 (ValueS Pure) (Insert (Proxy StateT3) Empty) m Safe val

--class PolyMonad5 m1 m2 m3 | m1 m2 -> m3 where
--    polyBind5 :: m1 a -> (X1 m1 a -> m2 c) -> m3 c

--type family X1 m a where
--    X1 (MonadCtx2 env1 set1 m1 s1)          a = a
--    X1 (ValueS m s)                         a = (ValueS Pure s a)
--    X1 (MonadCtx2Dummy env set m2 s2 m1 s1) a = (ValueS m2 s2 a)


--polyBind5 :: MonadCtx2 (ValueS Pure) (Insert (Proxy StateT3) Empty) m Safe (m2 c) -> (m2 c -> m2 c) -> m3 c <= MonadState4 (m2 c) m Safe


--tst3 = get5X `polyBind5` id - czemu to sie nie kompiluje?
--tst3 = (valS 0) `polyBind5` id - a to tak
--i analogicznie: runMonad2 ... $ liftF2' (+) get5X (valS 0)
--tst3 = get5X `polyBind5'` id - to tez sie nie kompiluje (tam jest prim). Co oznacza ze type family jest ok. 

--class PolyMonad5 m1 m2 m3 | m1 m2 -> m3 where
--    polyBind5 :: m1 a -> (X1 m1 a -> m2 c) -> m3 c

--    polyBind5 :: m1 a -> (m2 c -> m2 c) -> m3 c

--polyBind5 :: m1 (m2 c) -> (m2 c -> m2 c) -> m3 c



    --let x :: Int
    --    x = t1'
    ----t2' <- valS 6
    --valS 0
    --valS (+) <<*>> t1' <<*>> t2'

---
    --memSig_Vector_fstate = (mkArg :: NParam "self") // ()
    --memDef_Vector_fstate (self,()) = do
    --    x <- getX'
    --    putX' x

    --registerMethod ''Vector "fstate"

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





v = call2 $ appByName2 (Proxy :: Proxy "z") (valS 3) $ appNext2 (valS 2) $ appNext2 (valS 1) $ cons_Vector
    --a1 = call $ appNext (val 1) $ appNext (val 2) $ member (Proxy::Proxy "foo") v


    --mkLam lam sig = val $ appH (Lam lam) sig

mymain (self,()) = do
--    --mod <- call $ member (Proxy::Proxy "ModuleVector") (val Cls_ModuleVector)
--    --call $ member (Proxy::Proxy "main") mod


    v2 <- call2 $ appByName2 (Proxy :: Proxy "z") (valS 3) $ appNext2 (valS 2) $ appNext2 (valS 1) $ member2 (Proxy::Proxy "Vector") (valS Cls_Vector)

    let lam1 = mkLam (\x -> v2) ((mkArg :: NParam "self") // ())

    print_DBG $ call2 $ appNext2 (valS 1) lam1

    print_DBG v2

    print_DBG $ call2 $ appNext2 (valS 1) $ appNext2 (valS 2) $ member2 (Proxy::Proxy "foo") v2
    print_DBG $ call2 $ appByName2 (Proxy::Proxy "b") (valS 1) $ appNext2 (valS 2) $ member2 (Proxy::Proxy "foo") v2

    print_DBG $ call2 $ appNext2 (valS 2) $ appNext2 (valS 1) $ member2 (Proxy::Proxy "bar") v2
    print_DBG (valS "---")


    print_DBG $ call2 $ appNext2 (get5X) $ member2 (Proxy :: Proxy "baz") v2
    print_DBG $ call2 $ appNext2 (valS 5) $ member2 (Proxy :: Proxy "baz") v2


--    --print2 $ flip runStateTX (val 0) $ call $ member (Proxy :: Proxy "fstate") v2

    -- properties
    print_DBG $ call2 $ member2 (Proxy::Proxy "x") v2
    print_DBG $ call2 $ appNext2 (valS 5) $ member2 (Proxy::Proxy "x_setter") v2

----    --print'2 tst
--    print2 $ val "end"
--    print2 tstErr
--    print2 $ val "end2"
    valS 5


memSig_ModuleVector_main = (mkArg :: NParam "self") // ()
memDef_ModuleVector_main = mymain
registerMethod ''ModuleVector "main"


main = mainMaker2 cons_ModuleVector2




