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
--{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DysfunctionalDependencies #-}
!{-# LANGUAGE RightSideContexts #-}


module Luna.Target.HS.Host.Lift where

import Control.PolyApplicative.App 
import Luna.Target.HS.Control.Context
import Luna.Target.HS.Control.Error.Data
import Control.Monad.Shuffle
import Control.Category.Dot


------------------------------------------------------------------------
-- Type classes
------------------------------------------------------------------------

class AutoErrLift a b | a -> b where
    autoErrLift :: a -> b

class AutoEnvLift a b | a -> b where
    autoEnvLift :: a -> b


------------------------------------------------------------------------
-- Util lifting functions
------------------------------------------------------------------------

liftEnv0 = Value . Pure
liftEnv1 = app1 . Value . Pure
liftEnv2 = app2 . Value . Pure
liftEnv3 = app3 . Value . Pure
liftEnv4 = app4 . Value . Pure
liftEnv5 = app5 . Value . Pure


liftErr0 = Safe
liftErr1 = app1 . Safe
liftErr2 = app2 . Safe
liftErr3 = app3 . Safe
liftErr4 = app4 . Safe
liftErr5 = app5 . Safe


liftF0 = liftEnv0 . liftErr0
liftF1 = liftEnv1 . liftErr1
liftF2 = liftEnv2 . liftErr2
liftF3 = liftEnv3 . liftErr3
liftF4 = liftEnv4 . liftErr4
liftF5 = liftEnv5 . liftErr5


autoLift0 = (shuffleJoin . (fmap.fmap) autoEnvLift) `dot1` liftF0
autoLift1 = (shuffleJoin . (fmap.fmap) autoEnvLift) `dot2` liftF1
autoLift2 = (shuffleJoin . (fmap.fmap) autoEnvLift) `dot3` liftF2
autoLift3 = (shuffleJoin . (fmap.fmap) autoEnvLift) `dot4` liftF3
autoLift4 = (shuffleJoin . (fmap.fmap) autoEnvLift) `dot5` liftF4
autoLift5 = (shuffleJoin . (fmap.fmap) autoEnvLift) `dot6` liftF5

-- FIXME [wd]: automate with TH


------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance AutoErrLift (Safe a) (Safe a) where
    autoErrLift = id

instance AutoErrLift (UnsafeBase base err val) (UnsafeBase base err val) where
    autoErrLift = id

instance AutoErrLift a out <= out~Safe a where
    autoErrLift = Safe

---

instance AutoEnvLift (IO a) (Value IO a') <= AutoErrLift a a' where
    autoEnvLift = Value . fmap autoErrLift

instance AutoEnvLift (Pure a) (Value Pure a') <= AutoErrLift a a' where
    autoEnvLift = Value . fmap autoErrLift

instance AutoEnvLift (Value m a) (Value m a') <= (AutoErrLift a a', Functor m) where
    autoEnvLift = fmap autoErrLift

instance AutoEnvLift (MonadCtx base set m a) (MonadCtx base set m a') <= (AutoErrLift a a', Functor m) where
    autoEnvLift = fmap autoErrLift

instance AutoEnvLift a out <= (out~Value Pure a', AutoErrLift a a') where
    autoEnvLift = Value . Pure . autoErrLift


