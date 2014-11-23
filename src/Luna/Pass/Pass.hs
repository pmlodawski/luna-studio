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

module Luna.Pass.Pass where

import Control.Monad.Reader
import Control.Monad.State        hiding (fail, state)
import Control.Monad.Trans.Either

import Flowbox.Prelude           hiding (error, fail)
import Flowbox.System.Log.Logger
import Control.Monad.Trans.RWS (RWST, runRWST)



data Info = Info { name :: String
                 } deriving (Show)


logger :: Logger
logger = getLogger $(moduleName)


data Pass s f = Pass { _name :: String, _desc :: String, _state :: s, _func :: f }

type PassError = String

type PassT state result m = RWSTE Info [String] state PassError m result

type PassMonad state m = RWSTE Info [String] state PassError m

type Result m result = (PassCtx m) => m (Either PassError result)

type ResultT m = EitherT PassError m

type ESRT err env state m = EitherT err (StateT state (ReaderT env m))
type RWSTE r w s e m = RWST r w s (EitherT e m)

type PassCtx m = (Functor m, MonadIO m, Applicative m)

data NoState = NoState deriving Show


run :: env -> state -> RWSTE env [String] state err m result -> EitherT err m (result, state, [String])
run env state pass = runRWST pass env state

run0_ (Pass n d s f)       = run_ (Info n) s f
run1_ (Pass n d s f) t1    = run_ (Info n) s (f t1)
run2_ (Pass n d s f) t1 t2 = run_ (Info n) s (f t1 t2)

run_ :: (Monad m, Functor m) => env -> state -> RWSTE env [String] state err m result -> EitherT err m result
run_ env state pass = (\(x,_,_)->x) <$> run env state pass

fail :: Monad m => e -> EitherT e m a
fail = left













                -----------------------------------------------------------------------------
                ---- Copyright (C) Flowbox, Inc - All Rights Reserved
                ---- Unauthorized copying of this file, via any medium is strictly prohibited
                ---- Proprietary and confidential
                ---- Flowbox Team <contact@flowbox.io>, 2014
                -----------------------------------------------------------------------------

                --{-# LANGUAGE NoMonomorphismRestriction #-}
                --module Luna.Pass.Pass where

                --import           Flowbox.Prelude
                --import qualified Data.HMap as HMap
                --import           Data.HMap (HMap)
                --import           Control.Monad.Trans.Either (left, right)
                --import           Data.TypeLevel.List        (app)
                --import qualified Luna.Pass.Datastore as Datastore
                --import           Luna.Pass.Datastore (Datastore)
                --import           Luna.Pass.Data (DataInfo)


                ------------------------------------------------------------------------
                ---- Data types
                ------------------------------------------------------------------------

                --data Morph ins outs f = Morph ins outs f


                --runMorph (Morph ins outs f) dataStore = case args of 
                --    Left  desc -> left $ MissingData desc
                --    Right args -> do
                --        results <- app f args
                --        right $ Datastore.inserts outs results dataStore
                --    where args = Datastore.lookups ins dataStore


                --data Pass m = Pass {
                --                 _morph :: Datastore -> m Datastore
                --}

                --pass = Pass . runMorph



                --runPasses :: Monad m => [Pass m] -> Datastore -> m Datastore
                --runPasses ps dataStore = foldl (>>=) (return dataStore) (fmap _morph ps)



                --data PassError = MissingData [DataInfo]
                --               deriving (Show)