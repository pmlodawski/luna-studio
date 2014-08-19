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

--{-# LANGUAGE DysfunctionalDependencies #-}


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

---

instance PolyApplicative Pure (MonadCtx env set m) (MonadCtx env set m) <= Functor m where
    (Pure f) <<*>> a = f <$> a

instance PolyApplicative (MonadCtx env set m) Pure (MonadCtx env set m) <= (Functor m, Monad m) where
    f <<*>> (Pure a) = f <*> pure a


instance PolyApplicative IO (MonadCtx env set m) (MonadCtx envout set m) <= (MonadIO m, envout ~ EnvMerge env IO) where
    mf <<*>> ma = MonadCtx $ do
        f <- liftIO mf
        a <- fromMonadCtx ma
        return $ f a

instance PolyApplicative (MonadCtx env set m) IO (MonadCtx envout set m) <= (MonadIO m, envout ~ EnvMerge env IO) where
    mf <<*>> ma = MonadCtx $ do
        f <- fromMonadCtx mf
        a <- liftIO ma
        return $ f a

instance PolyApplicative (MonadCtx env1 set1 m1) (MonadCtx env2 set2 m2) (MonadCtx envout setout m1) <= (envout ~ EnvMerge env1 env2, setout ~ Union set1 set2, m1~m2, Monad m1) where
    mf <<*>> ma = MonadCtx $ do
        f <- fromMonadCtx mf
        a <- fromMonadCtx ma
        return $ f a

---------------------------

instance PolyApplicative (Value f) (Value a) (Value out) <= PolyApplicative f a out where
    Value f <<*>> Value a = Value $ f <<*>> a

instance PolyApplicative (Value f) (MonadCtx env set m) out <= PolyApplicative f (MonadCtx env set m) out where
    Value f <<*>> a = f <<*>> a

instance PolyApplicative (MonadCtx env set m) (Value ma) out <= PolyApplicative (MonadCtx env set m) ma out where
    f <<*>> Value a = f <<*>> a