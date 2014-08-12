---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE DysfunctionalDependencies #-}

!{-# LANGUAGE RightSideContexts #-}

module Luna.Target.HS.Control.Context.Bind where

import Control.PolyMonad
import Control.PolyApplicative
import Luna.Target.HS.Control.Context.Env
import Luna.Target.HS.Control.Context.MonadCtx
import Luna.Target.HS.Control.Context.Value
import Data.TypeLevel
import Control.Monad.IO.Class

----------------------------------------------------------------------------------
---- Utils
----------------------------------------------------------------------------------
bindEnv :: (PolyBindEnv m1 m2 m3 a1 a2, PolyMonad m1 m2 m3) =>
           m1 a1 -> (Value Pure a2 -> m2 b) -> m3 b
bindEnv = polyBindEnv

--bindEnv_ (a :: m1 a1) (b :: m2 b) = a `polyBindEnv` ((\_ -> b) :: (Value Pure a1 -> m2 b))
bindEnv_ a b = a `polyBindEnv` (\_ -> b)

--bindEnv_ (a :: m1 a1) (b :: m2 b) = a `polyBindEnv` ((\_ -> b) :: (Value Pure a1 -> m2 b))


--bindEnv :: m1 a -> (Value Pure a -> m2 b) -> m3 b <= PolyMonad m1 m2 m3
--bindEnv a f = a >>=~ (f . Value . Pure)

--bindEnv_ a b = bindEnv a (\_ -> b)



class PolyBindEnv m1 m2 m3 a1 a2 | m1 m2 -> m3, m1 a1 -> a2 where
    polyBindEnv :: PolyMonad m1 m2 m3 => m1 a1 -> (Value Pure a2 -> m2 b) -> m3 b


instance PolyBindEnv Pure m2 m3 a1 a2 <= (a1~a2) where
    a `polyBindEnv` f = a >>=~ (f . Value . Pure)

instance PolyBindEnv IO m2 m3 a1 a2 <= (a1~a2) where
    a `polyBindEnv` f = a >>=~ (f . Value . Pure)

instance PolyBindEnv (MonadCtx env set m) m2 m3 a1 a2 <= (a1 ~ Value Pure a2) where
    a `polyBindEnv` f = a >>=~ f

instance PolyBindEnv (Value m1) m2 m3 a1 a2 <= (PolyBindEnv m1 m2 m3 a1 a2, PolyMonad m1 m2 m3) where
    a `polyBindEnv` f = fromValue a `polyBindEnv` f


--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance PolyMonad Pure Pure Pure where
    a >>=~ f = f $ fromPure a

instance PolyMonad IO Pure IO where
    ma >>=~ f = ma >>= return . fromPure . f

instance PolyMonad Pure IO IO where
    a >>=~ f = f $ fromPure a

instance PolyMonad IO IO IO where
    a >>=~ f = a >>= f

---

instance PolyMonad (MonadCtx env1 set1 m1) (MonadCtx env2 set2 m2) (MonadCtx envout setout m1) <= (envout ~ EnvMerge env1 env2, setout ~ Union set1 set2, m1~m2, Monad m1) where
    a >>=~ f = MonadCtx $ (fromMonadCtx a) >>= (fromMonadCtx . f)

instance PolyMonad (MonadCtx env set m) Pure (MonadCtx envout set m) <= (envout ~ EnvMerge env Pure, Monad m) where
    a >>=~ f = MonadCtx $ (fromMonadCtx a) >>= (return . fromPure . f)

instance PolyMonad (MonadCtx env set m) IO (MonadCtx envout set m) <= (envout ~ EnvMerge env IO, MonadIO m) where
    a >>=~ f = MonadCtx $ (fromMonadCtx a) >>= (liftIO . f)

instance PolyMonad Pure (MonadCtx env set m) (MonadCtx env set m) where
    a >>=~ f = f $ fromPure a

instance PolyMonad IO (MonadCtx env set m) (MonadCtx envout set m) <= (envout ~ EnvMerge env IO, MonadIO m) where
    a >>=~ f = MonadCtx $ liftIO a >>= (fromMonadCtx . f)

---

instance PolyMonad (Value m1) m2 mout <= PolyMonad m1 m2 mout where
    a >>=~ f = fromValue a >>=~ f

instance PolyMonad m1 (Value m2) mout <= PolyMonad m1 m2 mout where
    a >>=~ f = a >>=~ (fromValue . f)

instance PolyMonad (Value m1) (Value m2) mout <= PolyMonad m1 m2 mout where
    a >>=~ f = fromValue a >>=~ (fromValue . f)

