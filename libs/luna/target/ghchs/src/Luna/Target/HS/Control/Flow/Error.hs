---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
!{-# LANGUAGE RightSideContexts #-}


module Luna.Target.HS.Control.Flow.Error (
    module Luna.Target.HS.Control.Flow.Error,
    module X
) where

import Luna.Target.HS.Control.Error   as X
import Luna.Target.HS.Control.Context

----------------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------------

instance Catch e (Value base1 a1) (Value base2 a2) (Value base3 a3) <= Catch e (base1 a1) (base2 a2) (base3 a3) where
    catch f a = Value $ catch (fromValue . f) (fromValue a)

instance Catch e (Pure a1) (Pure a2) (Pure a3) <= Catch e a1 a2 a3 where
    catch f a = Pure $ catch (fromPure . f) (fromPure a)

instance Raise e (Value base a) (Value base a') <= (Raise e a a', Functor base) where
    raise = fmap . raise

-- FIXME: we need other instances for catching here! For IO and MonadCtx