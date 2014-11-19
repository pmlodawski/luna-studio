---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}

module Luna.Pass2.Pass where

import Control.Monad.Reader
import Control.Monad.State        hiding (fail, state)
import Control.Monad.Trans.Either

import Flowbox.Prelude           hiding (error, fail)
import Flowbox.System.Log.Logger



--type PassError              = String

--type PassMonad    s m       = (Functor m, MonadRWS Info LogList s m)
--type PassMonadIO  s m       = (Functor m, MonadRWS Info LogList s m, MonadIO m)
--type Transformer  s b       = EitherT PassError (RWS Info LogList s) b
--type TransformerT s a m b   = EitherT a (RWST Info LogList s m) b
--type Result       m output  = ResultT m output
--type ResultT      m         = EitherT PassError m

--data NoState = NoState deriving (Show)


data Info = Info { name :: String
                 } deriving (Show)


logger :: Logger
logger = getLogger $(moduleName)


type PassError = String

type PassT state result m = ESRT PassError Info state m result

type Pass state result = (MonadIO m) => ESRT PassError Info state m result

type Result result = (Functor m, MonadIO m) => m (Either PassError result)

type ResultT m = EitherT PassError m

type ESRT err env state m = EitherT err (StateT state (ReaderT env m))

data NoState = NoState deriving Show


runRaw :: ESRT err env state m result -> env -> state -> m (Either err result, state)
runRaw pass env state = flip runReaderT env $ flip runStateT state $ (runEitherT) pass


run :: Monad m => env -> state -> ESRT err env state m result -> m (Either err (result, state))
run env state pass = do
    (r, rstate) <- runRaw pass env state
    return $ fmap (,rstate) r


run_ :: (Monad m, Functor m) => env -> state -> ESRT err env state m result -> m (Either err result)
run_ env state pass = fmap fst <$> run env state pass


runHoist :: Monad m => env -> state -> ESRT err env state (EitherT err m) result -> EitherT err m (result, state)
runHoist env state pass = hoistEither =<< run env state pass


runHoist_ :: Monad m => env -> state -> ESRT err env state (EitherT err m) result -> EitherT err m result
runHoist_ env state pass = fst <$> runHoist env state pass


fail :: Monad m => e -> EitherT e m a
fail = left




--data Pass