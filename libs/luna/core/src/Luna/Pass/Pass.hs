---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
--{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}

module Luna.Pass.Pass where

import           Control.Monad.Reader
import           Control.Monad.State        hiding (fail, state)
import           Control.Monad.Trans.Either

import           Control.Monad.Trans.RWS    (RWST, runRWST)
import           Flowbox.Prelude            hiding (error, fail)
import           Flowbox.System.Log.Logger
import           Luna.System.Session        (SessionMonad)


data Info = Info { name :: String
                 } deriving (Show)


logger :: Logger
logger = getLogger $moduleName


data Pass s f = Pass { _name :: String, _desc :: String, _state :: s, _func :: f }

type PassError = String

type PassT state result m = RWSTE Info [String] state PassError m result

type PassMonad state m = RWSTE Info [String] state PassError m

--type Result m result = (PassCtx m) => m (Either PassError result)
type Result2 m result = m (Either PassError result)

type ResultT m = EitherT PassError m

type ESRT err env state m = EitherT err (StateT state (ReaderT env m))
type RWSTE r w s e m = RWST r w s (EitherT e m)

type PassCtx m = (Functor m, MonadIO m, Applicative m, SessionMonad m)

data NoState = NoState deriving Show


run :: SessionMonad m => env -> state -> RWSTE env [String] state err m result -> EitherT err m (result, state, [String])
run env state pass = runRWST pass env state

run0_ (Pass n d s f)          = run_ (Info n) s f
run1_ (Pass n d s f) t1       = run_ (Info n) s (f t1)
run2_ (Pass n d s f) t1 t2    = run_ (Info n) s (f t1 t2)
run3_ (Pass n d s f) t1 t2 t3 = run_ (Info n) s (f t1 t2 t3)

run_ :: (Monad m, Functor m, SessionMonad m) => env -> state -> RWSTE env [String] state err m result -> EitherT err m result
run_ env state pass = (\(x,_,_)->x) <$> run env state pass

fail :: (Monad m, Monoid w) => e -> RWSTE r w s e m a
fail = lift . left
