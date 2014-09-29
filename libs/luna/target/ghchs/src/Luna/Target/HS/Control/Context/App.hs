---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}
!{-# LANGUAGE RightSideContexts #-}

{-# LANGUAGE DysfunctionalDependencies #-}


module Luna.Target.HS.Control.Context.App where

import Control.PolyMonad
import Control.Applicative
import Control.PolyApplicative
import Luna.Target.HS.Control.Context.Env
import Luna.Target.HS.Control.Context.MonadCtx
import Luna.Target.HS.Control.Context.Value
import Data.TypeLevel
import Control.Monad.IO.Class

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance PolyApplicative Pure Pure Pure where
    Pure f <<*>> Pure a = Pure $ f a

instance PolyApplicative IO Pure IO where
    f <<*>> Pure a = f <*> return a

instance PolyApplicative Pure IO IO where
    Pure f <<*>> a = a >>= (return . f)

instance PolyApplicative IO IO IO where
    f <<*>> a = f <*> a

-----------------------------------

instance PolyApplicative (Value Pure s1) (Value Pure s2) (Value Pure s3) <= PolyApplicative s1 s2 s3 where
    Value (Pure sf) <<*>> Value (Pure sa) = Value . Pure $ sf <<*>> sa

instance PolyApplicative (Value IO s1) (Value Pure s2) (Value IO s3) <= PolyApplicative s1 s2 s3 where
    Value msf <<*>> Value (Pure sa) = Value $ do
        sf <- msf
        return $ sf <<*>> sa

instance PolyApplicative (Value Pure s1) (Value IO s2) (Value IO s3) <= PolyApplicative s1 s2 s3 where
    Value (Pure sf) <<*>> Value msa = Value $ do
        sa <- msa
        return $ sf <<*>> sa

instance PolyApplicative (Value IO s1) (Value IO s2) (Value IO s3) <= PolyApplicative s1 s2 s3 where
    Value msf <<*>> Value msa = Value $ do
        sf <- msf
        sa <- msa
        return $ sf <<*>> sa


