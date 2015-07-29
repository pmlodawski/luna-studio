---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RebindableSyntax #-}
--{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DysfunctionalDependencies #-}



module Luna.Target.HS.Host.Lift where

import Control.PolyApplicative.App 
import Control.PolyApplicative
import Luna.Target.HS.Control.Context
import Luna.Target.HS.Control.Error.Data
import Control.Monad.Shuffle
import Control.Category.Dot
import Luna.Target.HS.Control.Flow.Env
import Luna.Target.HS.Host.Rebindable
import Control.PolyMonad
import Data.TupleList hiding (uncurryTuple)
import Data.RTuple           (RTuple(RTuple), fromRTuple, uncurryTuple)

------------------------------------------------------------------------
-- Type classes
------------------------------------------------------------------------

class AutoLift a b | a -> b where
    autoLift :: a -> b


------------------------------------------------------------------------
-- Util lifting functions
------------------------------------------------------------------------


liftF0 f = val f
liftF1 f t1 = do
    t1' <- t1
    val f <<*>> t1'

--liftF1'x t1 = do
--    t1' <- t1
--    t1'

--zzz =  liftF1'x get5X

liftF2 f t1 t2 = do
    t1' <- t1
    t2' <- t2
    val f <<*>> t1' <<*>> t2'

--testi2 a b = a >>>~ (\_ -> b)
--{-# INLINE testi2 #-}


--tx1 = liftF2'x (,) get5X  (val 2)
--tx2 = liftF2' (,) get5X  (val 2)


liftF3 f t1 t2 t3 = do
    t1' <- t1
    t2' <- t2
    t3' <- t3
    val f <<*>> t1' <<*>> t2' <<*>> t3'


liftF4 f t1 t2 t3 t4 = do
    t1' <- t1
    t2' <- t2
    t3' <- t3
    t4' <- t4
    val f <<*>> t1' <<*>> t2' <<*>> t3' <<*>> t4'

liftF5 f t1 t2 t3 t4 t5 = do
    t1' <- t1
    t2' <- t2
    t3' <- t3
    t4' <- t4
    t5' <- t5
    val f <<*>> t1' <<*>> t2' <<*>> t3' <<*>> t4' <<*>> t5'

liftF6 f t1 t2 t3 t4 t5 t6 = do
    t1' <- t1
    t2' <- t2
    t3' <- t3
    t4' <- t4
    t5' <- t5
    t6' <- t6
    val f <<*>> t1' <<*>> t2' <<*>> t3' <<*>> t4' <<*>> t5' <<*>> t6'

liftF7 f t1 t2 t3 t4 t5 t6 t7 = do
    t1' <- t1
    t2' <- t2
    t3' <- t3
    t4' <- t4
    t5' <- t5
    t6' <- t6
    t7' <- t7
    val f <<*>> t1' <<*>> t2' <<*>> t3' <<*>> t4' <<*>> t5' <<*>> t6' <<*>> t7'

liftF8 f t1 t2 t3 t4 t5 t6 t7 t8 = do
    t1' <- t1
    t2' <- t2
    t3' <- t3
    t4' <- t4
    t5' <- t5
    t6' <- t6
    t7' <- t7
    t8' <- t8
    val f <<*>> t1' <<*>> t2' <<*>> t3' <<*>> t4' <<*>> t5' <<*>> t6' <<*>> t7' <<*>> t8'

liftF9 f t1 t2 t3 t4 t5 t6 t7 t8 t9 = do
    t1' <- t1
    t2' <- t2
    t3' <- t3
    t4' <- t4
    t5' <- t5
    t6' <- t6
    t7' <- t7
    t8' <- t8
    t9' <- t9
    val f <<*>> t1' <<*>> t2' <<*>> t3' <<*>> t4' <<*>> t5' <<*>> t6' <<*>> t7' <<*>> t8' <<*>> t9'

liftF10 f t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 = do
    t1' <- t1
    t2' <- t2
    t3' <- t3
    t4' <- t4
    t5' <- t5
    t6' <- t6
    t7' <- t7
    t8' <- t8
    t9' <- t9
    t10' <- t10
    val f <<*>> t1' <<*>> t2' <<*>> t3' <<*>> t4' <<*>> t5' <<*>> t6' <<*>> t7' <<*>> t8' <<*>> t9' <<*>> t10'

liftF11 f t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 = do
    t1' <- t1
    t2' <- t2
    t3' <- t3
    t4' <- t4
    t5' <- t5
    t6' <- t6
    t7' <- t7
    t8' <- t8
    t9' <- t9
    t10' <- t10
    t11' <- t11
    val f <<*>> t1' <<*>> t2' <<*>> t3' <<*>> t4' <<*>> t5' <<*>> t6' <<*>> t7' <<*>> t8' <<*>> t9' <<*>> t10' <<*>> t11'

liftF12 f t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 = do
    t1' <- t1
    t2' <- t2
    t3' <- t3
    t4' <- t4
    t5' <- t5
    t6' <- t6
    t7' <- t7
    t8' <- t8
    t9' <- t9
    t10' <- t10
    t11' <- t11
    t12' <- t12
    val f <<*>> t1' <<*>> t2' <<*>> t3' <<*>> t4' <<*>> t5' <<*>> t6' <<*>> t7' <<*>> t8' <<*>> t9' <<*>> t10' <<*>> t11' <<*>> t12'

liftFR0 = curryTuple0 . liftF0 -- :: a                     -> ()                           -> Value Pure Safe a
liftFR1 = curryTuple1 . liftF1 -- :: (a1 -> c)             -> (m1 a, ())                   -> XOut m1 m2 c
liftFR2 = curryTuple2 . liftF2 -- :: (a3 -> a2 -> c)       -> (m1 a, (m2 a1, ()))          -> XOut m1 (XOut m2 m3) c
liftFR3 = curryTuple3 . liftF3 -- :: (a5 -> a4 -> a3 -> c) -> (m1 a, (m2 a1, (m3 a2, ()))) -> XOut m1 (XOut m2 (XOut m3 m4)) c
liftFR4 = curryTuple4 . liftF4 
liftFR5 = curryTuple5 . liftF5
liftFR6 = curryTuple6 . liftF6
liftFR7 = curryTuple7 . liftF7
liftFR8 = curryTuple8 . liftF8
liftFR9 = curryTuple9 . liftF9
liftFR10 = curryTuple10 . liftF10
liftFR11 = curryTuple11 . liftF11
liftFR12 = curryTuple12 . liftF12


--liftCons0 f = liftFR0 f . snd -- :: a                     -> (_,())                           -> Value Pure Safe a
--liftCons1 f = liftFR1 f . snd -- :: (a1 -> c)             -> (_,(m1 a, ()))                   -> XOut m1 m2 c
--liftCons2 f = liftFR2 f . snd -- :: (a3 -> a2 -> c)       -> (_,(m1 a, (m2 a1, ())))          -> XOut m1 (XOut m2 m3) c
--liftCons3 f = liftFR3 f . snd -- :: (a5 -> a4 -> a3 -> c) -> (_,(m1 a, (m2 a1, (m3 a2, ())))) -> XOut m1 (XOut m2 (XOut m3 m4)) c
--liftCons4 f = liftFR4 f . snd
--liftCons5 f = liftFR5 f . snd

liftCons0 v _ = liftFlatF0 v
liftCons1 v _ = liftFlatF1 v
liftCons2 v _ = liftFlatF2 v
liftCons3 v _ = liftFlatF3 v
liftCons4 v _ = liftFlatF4 v
liftCons5 v _ = liftFlatF5 v
liftCons6 v _ = liftFlatF6 v
liftCons7 v _ = liftFlatF7 v
liftCons8 v _ = liftFlatF8 v
liftCons9 v _ = liftFlatF9 v
liftCons10 v _ = liftFlatF10 v
liftCons11 v _ = liftFlatF11 v
liftCons12 v _ = liftFlatF12 v

liftFlatF0 = flatF0 . liftF0
liftFlatF1 = flatF1 . liftF1
liftFlatF2 = flatF2 . liftF2
liftFlatF3 = flatF3 . liftF3
liftFlatF4 = flatF4 . liftF4
liftFlatF5 = flatF5 . liftF5
liftFlatF6 = flatF6 . liftF6
liftFlatF7 = flatF7 . liftF7
liftFlatF8 = flatF8 . liftF8
liftFlatF9 = flatF9 . liftF9
liftFlatF10 = flatF10 . liftF10
liftFlatF11 = flatF11 . liftF11
liftFlatF12 = flatF12 . liftF12


--liftCons0 = curryTuple1 . const . liftF0
--liftCons1 = curryTuple2 . const . liftF1
--liftCons2 = curryTuple3 . const . liftF2
--liftCons3 = curryTuple4 . const . liftF3
--liftCons4 = curryTuple5 . const . liftF4
--liftCons5 = curryTuple6 . const . liftF5

--liftCons2' = curryTuple3 . liftF2
--liftCons3' = curryTuple4 . liftF3
--liftCons4' = curryTuple5 . liftF4
--liftCons5' = curryTuple6 . liftF5

--liftEnv0 = Value . Pure
--liftEnv1 = app1 . Value . Pure
--liftEnv2 = app2 . Value . Pure
--liftEnv3 = app3 . Value . Pure
--liftEnv4 = app4 . Value . Pure
--liftEnv5 = app5 . Value . Pure
--liftEnv6 = app6 . Value . Pure
--liftEnv7 = app7 . Value . Pure
--liftEnv8 = app8 . Value . Pure
--liftEnv9 = app9 . Value . Pure


--liftEnv0' = Value . Pure
--liftEnv1' = app1 . Value . Pure
--liftEnv2' = app2 . Value . Pure
--liftEnv3' = app3 . Value . Pure
--liftEnv4' = app4 . Value . Pure
--liftEnv5' = app5 . Value . Pure
--liftEnv6' = app6 . Value . Pure
--liftEnv7' = app7 . Value . Pure
--liftEnv8' = app8 . Value . Pure
--liftEnv9' = app9 . Value . Pure


liftErr0 = Safe
liftErr1 = app1 . Safe
liftErr2 = app2 . Safe
liftErr3 = app3 . Safe
liftErr4 = app4 . Safe
liftErr5 = app5 . Safe
liftErr6 = app6 . Safe
liftErr7 = app7 . Safe
liftErr8 = app8 . Safe
liftErr9 = app9 . Safe
liftErr10 = app10 . Safe
liftErr11 = app11 . Safe
liftErr12 = app12 . Safe


--liftErr0' = Safe
--liftErr1' = app1 . Safe
--liftErr2' = app2 . Safe
--liftErr3' = app3 . Safe
--liftErr4' = app4 . Safe
--liftErr5' = app5 . Safe
--liftErr6' = app6 . Safe
--liftErr7' = app7 . Safe
--liftErr8' = app8 . Safe
--liftErr9' = app9 . Safe


--liftF0 = liftEnv0 . liftErr0
--liftF1 = liftEnv1 . liftErr1
--liftF2 = liftEnv2 . liftErr2
--liftF3 = liftEnv3 . liftErr3
--liftF4 = liftEnv4 . liftErr4
--liftF5 = liftEnv5 . liftErr5
--liftF6 = liftEnv6 . liftErr6
--liftF7 = liftEnv7 . liftErr7
--liftF8 = liftEnv8 . liftErr8




--liftF0' = liftEnv0' . liftErr0'
--liftF1' = liftEnv1' . liftErr1'
--liftF2' = liftEnv2' . liftErr2'
--liftF3' = liftEnv3' . liftErr3'
--liftF4' = liftEnv4' . liftErr4'
--liftF5' = liftEnv5' . liftErr5'
--liftF6' = liftEnv6' . liftErr6'
--liftF7' = liftEnv7' . liftErr7'
--liftF8' = liftEnv8' . liftErr8'


-- FIXME [wd]: update
--autoLift0 = (shuffleJoin . (fmap.fmap) autoEnvLift) `dot1` liftF0
--autoLift1 = (shuffleJoin . (fmap.fmap) autoEnvLift) `dot2` liftF1
--autoLift2 = (shuffleJoin . (fmap.fmap) autoEnvLift) `dot3` liftF2
--autoLift3 = (shuffleJoin . (fmap.fmap) autoEnvLift) `dot4` liftF3
--autoLift4 = (shuffleJoin . (fmap.fmap) autoEnvLift) `dot5` liftF4
--autoLift5 = (shuffleJoin . (fmap.fmap) autoEnvLift) `dot6` liftF5

-- FIXME [wd]: update
--liftCons0 = curryTuple1 . const . liftF0
--liftCons1 = curryTuple2 . const . liftF1
--liftCons2 = curryTuple3 . const . liftF2
--liftCons3 = curryTuple4 . const . liftF3
--liftCons4 = curryTuple5 . const . liftF4
--liftCons5 = curryTuple6 . const . liftF5
--liftCons6 = curryTuple7 . const . liftF6
--liftCons7 = curryTuple8 . const . liftF7
--liftCons8 = curryTuple9 . const . liftF8

autoLift1 = (polyJoin . fmap autoLift) `dot2` liftF1
autoLift2 = (polyJoin . fmap autoLift) `dot3` liftF2
autoLift3 = (polyJoin . fmap autoLift) `dot4` liftF3
autoLift4 = (polyJoin . fmap autoLift) `dot5` liftF4
autoLift5 = (polyJoin . fmap autoLift) `dot6` liftF5
autoLift6 = (polyJoin . fmap autoLift) `dot7` liftF6
autoLift7 = (polyJoin . fmap autoLift) `dot8` liftF7
autoLift8 = (polyJoin . fmap autoLift) `dot9` liftF8
autoLift9 = (polyJoin . fmap autoLift) `dot10` liftF9
autoLift10 = (polyJoin . fmap autoLift) `dot11` liftF10
autoLift11 = (polyJoin . fmap autoLift) `dot12` liftF11
autoLift12 = (polyJoin . fmap autoLift) `dot13` liftF12


-- FIXME [wd]: automate with TH


------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------


instance AutoLift (Value base safety a) (Value base safety a) where
    autoLift = id

instance AutoLift (IO a) (Value IO Safe a) where
    autoLift = Value . fmap Safe

instance AutoLift (Pure a) (Value Pure Safe a) where
    autoLift = Value . fmap Safe

instance AutoLift (Safe a) (Value Pure Safe a) where
    autoLift = Value . Pure

instance AutoLift (UnsafeBase base err a) (Value Pure (UnsafeBase base err) a) where
    autoLift = Value . Pure




--instance AutoErrLift (Safe a) (Safe a) where
--    autoErrLift = id

--instance AutoErrLift (UnsafeBase base err val) (UnsafeBase base err val) where
--    autoErrLift = id

--instance  out~Safe a =>AutoErrLift a out  where
--    autoErrLift = Safe

---

-- FIXME [wd]: update
--instance  AutoErrLift a a' =>AutoEnvLift (IO a) (Value IO a')  where
--    autoEnvLift = Value . fmap autoErrLift

--instance  AutoErrLift a a' =>AutoEnvLift (Pure a) (Value Pure a')  where
--    autoEnvLift = Value . fmap autoErrLift

--instance  (AutoErrLift a a', Functor m) =>AutoEnvLift (Value m a) (Value m a')  where
--    autoEnvLift = fmap autoErrLift

--instance  (AutoErrLift a a', Functor m) =>AutoEnvLift (MonadCtx base set m a) (MonadCtx base set m a')  where
--    autoEnvLift = fmap autoErrLift

--instance  (out~Value Pure a', AutoErrLift a a') =>AutoEnvLift a out  where
--    autoEnvLift = Value . Pure . autoErrLift





------------------------------------------------------


----------------------------------------------------------------------
-- Flattening
-- It is used to flatten all env data (IO/Pure + Safe/Unsafe) in order to store the data in datatypes
-- Currently datatypes cannot store env-tagged data, althought tuples are able to.
--
-- Example:
-- (1,2) is in reality val (val 1, val 2)
-- where Foo (1,2) is in reality val (Foo (1,2))
----------------------------------------------------------------------

flatF0 f                = f
flatF1 f                = flatF0 f             . flattenEnv
flatF2 f t1             = flatF1 f t1          . flattenEnv
flatF3 f t1 t2          = flatF2 f t1 t2       . flattenEnv
flatF4 f t1 t2 t3       = flatF3 f t1 t2 t3    . flattenEnv
flatF5 f t1 t2 t3 t4    = flatF4 f t1 t2 t3 t4 . flattenEnv
flatF6 f t1 t2 t3 t4 t5 = flatF5 f t1 t2 t3 t4 t5 . flattenEnv
flatF7 f t1 t2 t3 t4 t5 t6 = flatF6 f t1 t2 t3 t4 t5 t6 . flattenEnv
flatF8 f t1 t2 t3 t4 t5 t6 t7 = flatF7 f t1 t2 t3 t4 t5 t6 t7 . flattenEnv
flatF9 f t1 t2 t3 t4 t5 t6 t7 t8 = flatF8 f t1 t2 t3 t4 t5 t6 t7 t8 . flattenEnv
flatF10 f t1 t2 t3 t4 t5 t6 t7 t8 t9 = flatF9 f t1 t2 t3 t4 t5 t6 t7 t8 t9 . flattenEnv
flatF11 f t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 = flatF10 f t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 . flattenEnv
flatF12 f t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 = flatF11 f t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 . flattenEnv

class FlattenEnv ma a mb b | ma a -> mb b where
    flattenEnv :: ma a -> mb b

instance (ma~mb, a~b) => FlattenEnv ma a mb b where
    flattenEnv = id

instance (Functor ma, Functor m, FlattenEl a m b, PolyMonad ma m mb) => FlattenEnv ma (RTuple a) mb (RTuple b) where
    flattenEnv = polyJoin . fmap flattenEl


class FlattenEl a m b | a -> m b where
    flattenEl :: a -> m b

instance FlattenEl () (Value Pure Safe) () where
    flattenEl = val

instance (Functor mt, FlattenEl ts m20 ts', PolyApplicative mt m20 m) => FlattenEl (mt t, ts) m (t, ts') where
    flattenEl (mt, ts) = fmap (,) mt <<*>> flattenEl ts

instance (Functor m, FlattenEl a m b) => FlattenEl (RTuple a) m (RTuple b) where
    flattenEl (RTuple a) = fmap RTuple $ flattenEl a



expandEnv = val . expandEl


class ExpandEl a b | a -> b where
    expandEl :: a -> b

instance (a~b) => ExpandEl a b where
    expandEl = id

instance ExpandEl (RTuple ()) (RTuple ()) where
    expandEl = id

instance (ExpandEl a b, ExpandEl (RTuple as) (RTuple bs)) => ExpandEl (RTuple (a,as)) (RTuple (Value Pure Safe b,bs)) where
    expandEl (RTuple (a,as)) = RTuple (expandEnv a, fromRTuple . expandEl $ RTuple as)

--main = print $ expandEnv $ RTuple(1::Int,(RTuple(2::Int,()),()))



--main = print $ flattenEnv $ val $ RTuple (val(1::Int),(val(2::Int),()))
--main = print $ polyJoin $ val $ val (1::Int) 
--zaczelo dzialac bo do binda przenioslem instancje monadSafety z BaseMonads!!!

--instance FlattenEnv ma (RTuple (mt t, ts)) ma (RTuple (t, ts')) where
--    flattenEnv mrt = polyJoin $ fmap (fst . fromRTuple) mrt
        --RTuple (mt, ts) <- mrt
        --t <- mt
        --ts' <- flattenEnv ts
        --return (t, ts')

        --JAK ZROBIC POPRAWNIE FLATTENENV?

--flattenEnv2 :: a
--flattenEnv2 (a,as) = fmap (,) a <<*>> as
