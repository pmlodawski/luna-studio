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

--------------------------------------------------------------------------------
-- Structures
--------------------------------------------------------------------------------

newtype Req req m a = Req (m a) deriving (Show, Typeable)

fromReq (Req a) = a

newtype MonadCtx (base :: * -> *) set m val = MonadCtx (m val) deriving (Show, Typeable)

fromMonadCtx :: MonadCtx base set m val -> m val
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

closeMonadCtx :: MonadCtx base set base a -> Value base a
closeMonadCtx (MonadCtx a) = Value a

runMonadProto :: mptr -> (ma a -> mb b) -> (MonadCtx env set ma a) -> (MonadCtx env (Remove mptr set) mb b)
runMonadProto _ f ms = MonadCtx $ f (fromMonadCtx ms)

runMonadProtoReq :: mptr -> (ma a -> mb b) -> Req mptr (MonadCtx env set ma) a -> MonadCtx env (Remove mptr set) mb b
runMonadProtoReq mptr f ms = runMonadProto mptr f (fromReq ms)

runMonad :: mptr -> (ma a -> mb b) -> Req mptr (MonadCtx env set ma) a -> t b <= MatchMonadClose (MonadCtx env (Remove mptr set) mb) t
runMonad = matchMonadClose `dot3` runMonadProtoReq



--runMonad'' :: mptr -> (ma a -> mb b) -> MonadCtx env (ConstrainSet (Insert mptr Empty) set) ma a -> t b <= MatchMonadClose (MonadCtx env (ConstrainSet (Insert mptr Empty) (Remove mptr set)) mb) t 
    --runMonad'' = (removeReqConstrains . matchMonadClose) `dot3` runMonadProto'

--liftMonadRunner1' :: MatchMonadCloseProto (IsEmpty (Remove mptr set)) (MonadCtx env (Remove mptr set) mb) t => mptr -> (ma a1 -> a -> mb b) -> MonadCtx env (Insert (Include mptr) Empty) ma a1 -> a -> t b
--liftMonadRunner1' (mptr :: mprt) (f :: ma a1 -> a -> mb b) (m :: MonadCtx env (Insert (Include mptr) Empty) ma a1) = flip (runMonad mptr) m . (appLastArg1 f)

liftMonadRunner1 mptr f m = flip (runMonad mptr) m . (appLastArg1 f)
liftMonadRunner2 mptr f m = flip (runMonad mptr) m . (appLastArg2 f)
liftMonadRunner3 mptr f m = flip (runMonad mptr) m . (appLastArg3 f)


--removeReqConstrains :: MonadCtx env (ConstrainSet req set) m a -> MonadCtx env (ConstrainSet newreq set) m a
--removeReqConstrains = MonadCtx . fromMonadCtx




--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Monad (MonadCtx base set m) <= Monad m where
    return = MonadCtx . return
    (MonadCtx ma) >>= f = MonadCtx $ do
        a <- ma
        fromMonadCtx $ f a

instance Functor (MonadCtx base set m) <= Functor m where
    fmap f (MonadCtx mval) = MonadCtx $ fmap f mval

instance Applicative (MonadCtx base set m) <= (Monad m, Functor m) where
    pure = MonadCtx . return
    MonadCtx mf <*> MonadCtx ma = MonadCtx $ do
        f <- mf
        a <- ma
        return $ f a
    
        
instance MatchMonadCloseProto False m m where
    matchMonadCloseProto _ = id

instance MatchMonadCloseProto True (MonadCtx env set m) (Value env) <= (m~env) where
    matchMonadCloseProto _ = closeMonadCtx

instance MatchMonadClose (MonadCtx env set ma) out <= (MatchMonadCloseProto emptySet (MonadCtx env set ma) out, emptySet ~ IsEmpty set) where
    matchMonadClose = matchMonadCloseProto (undefined :: emptySet)
---

--instance MatchMonadClose (MonadCtx env set ma) out <= out ~ MonadCtx env set ma where
--    matchMonadClose = id

--instance MatchMonadClose (MonadCtx env () m) env <= (m~env) where
--    matchMonadClose = closeMonadCtx

---

--instance MatchMonadCloseProto False m m where
--    matchMonadCloseProto _ = id

--instance MatchMonadCloseProto True (MonadCtx env set m) (Value env) <= (m~env) where
--    matchMonadCloseProto _ = closeMonadCtx

--instance MatchMonadClose (MonadCtx env (ConstrainSet req set) ma) out <= (MatchMonadCloseProto emptySet (MonadCtx env (ConstrainSet req set) ma) out, emptySet ~ IsEmpty set) where
--    matchMonadClose = matchMonadCloseProto (undefined :: emptySet)

---

instance AppMonadCtx (MonadCtx env set m a) (Req req (MonadCtx env set m) a) where
    appMonadCtx = Req

instance AppMonadCtx (Req req (MonadCtx env set m) a) (Req req (MonadCtx env set m) a) where
    appMonadCtx = id

instance AppMonadCtx (Value m2 a2) (Req req (MonadCtx env set m1) a1) <= (a1~Value Pure a2, env~m2, set~Insert req Empty, m1~t m2, MonadTrans t, Monad m2, Functor m2) where
    appMonadCtx = Req . MonadCtx . lift . (fmap (Value . Pure)) . fromValue

---

instance IOEnv (Value m) <= IOEnv m where
    toIOEnv = toIOEnv . fromValue

instance IOEnv (MonadCtx base set m) <= (base~m, IOEnv m) where
    toIOEnv = toIOEnv . closeMonadCtx