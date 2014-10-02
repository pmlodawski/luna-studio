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
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE DeriveFunctor #-}

{-# LANGUAGE DysfunctionalDependencies #-}

!{-# LANGUAGE RightSideContexts #-}

module Luna.Target.HS.Control.Context.MonadCtx where

import Control.Monad.Trans (MonadTrans, lift)
import Control.PolyMonad
import Control.PolyApplicative
import Luna.Target.HS.Control.Context.Env
import Luna.Target.HS.Control.Context.Value
import Control.Monad.IO.Class
import Data.Typeable (Typeable)
import Flowbox.Utils
import Data.TypeLevel
import Control.Applicative

import Luna.Target.HS.Control.Error.Data as DELME

--------------------------------------------------------------------------------
-- Structures
--------------------------------------------------------------------------------

newtype Req req m a = Req (m a) deriving (Show, Typeable)
fromReq (Req a) = a

newtype MonadCtx (base :: (* -> *) -> * -> *) set (m :: (* -> *) -> * -> *) s val = MonadCtx (m s val) deriving (Show, Typeable, Functor)
fromMonadCtx (MonadCtx a) = a


--------------------------------------------------------------------------------
-- Type classes
--------------------------------------------------------------------------------

class MatchMonadCloseProto flag m t | flag m -> t where
    matchMonadCloseProto :: flag -> m a -> t a

class MatchMonadClose m t | m -> t where
    matchMonadClose :: m a -> t a

class AppMonadCtx a b | a -> b where
    appMonadCtx :: a -> b


--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

closeMonadCtx :: MonadCtx base set base s a -> base s a
closeMonadCtx (MonadCtx a) = a

runMonadProto :: mptr -> (ma sa a -> mb sb b) -> (MonadCtx env set ma sa a) -> (MonadCtx env (Remove mptr set) mb sb b)
runMonadProto _ f ms = MonadCtx $ f (fromMonadCtx ms)

runMonadProtoReq :: mptr -> (ma sa a -> mb sb b) -> Req mptr (MonadCtx env set ma sa) a -> MonadCtx env (Remove mptr set) mb sb b
runMonadProtoReq mptr f ms = runMonadProto mptr f (fromReq ms)

runMonad :: mptr -> (ma sa a -> mb sb b) -> Req mptr (MonadCtx env set ma sa) a -> t b <= MatchMonadClose (MonadCtx env (Remove mptr set) mb sb) t
runMonad = matchMonadClose `dot3` runMonadProtoReq


-- FIXME [wd]: to update
--liftMonadRunner1 mptr f m = flip (runMonad mptr) m . (appArg2 f)
--liftMonadRunner2 mptr f m = flip (runMonad mptr) m . (appArg3 f)
--liftMonadRunner3 mptr f m = flip (runMonad mptr) m . (appArg4 f)


--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance MatchMonadCloseProto False m m where
    matchMonadCloseProto _ = id

instance MatchMonadCloseProto True (MonadCtx env set m s) (env s) <= env~m where
    matchMonadCloseProto _ = closeMonadCtx

instance MatchMonadClose (MonadCtx env set ma sa) out <= (MatchMonadCloseProto emptySet (MonadCtx env set ma sa) out, emptySet ~ IsEmpty set) where
    matchMonadClose = matchMonadCloseProto (undefined :: emptySet)

---

instance AppMonadCtx (MonadCtx env set m s a) (Req req (MonadCtx env set m s) a) where
    appMonadCtx = Req

instance AppMonadCtx (Value m2 s2 a2) (Req req (MonadCtx env set m1 s1) a1) <= (set~Insert req Empty, s1~s2, m1~t (Value m2), a1 ~ Value Pure Safe a2, LiftValue' (Value m2) s2 t, Functor s2, Functor m2) where
    appMonadCtx = Req . MonadCtx . liftValue' . (fmap (Value . Pure . Safe))

