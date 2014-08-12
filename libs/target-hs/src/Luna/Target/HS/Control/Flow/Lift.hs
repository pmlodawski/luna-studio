---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}


module Luna.Target.HS.Control.Flow.Lift where

import Control.PolyApplicative.App 
import Luna.Target.HS.Control.Context.Env
import Luna.Target.HS.Control.Context.Value
import Luna.Target.HS.Control.Error.Data

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


-- FIXME [wd]: automate with TH





