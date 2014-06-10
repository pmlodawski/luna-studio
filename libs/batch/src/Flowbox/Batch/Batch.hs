---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE Rank2Types #-}

module Flowbox.Batch.Batch (
      module X
    , Error
    , Batch
    , BatchEnv(..)
    , runBatch
    , make
    , attributeKey
) where

import Control.Monad.State        as X
import Control.Monad.Trans.Either as X

import           Flowbox.Batch.Project.ProjectManager (ProjectManager)
import qualified Flowbox.Batch.Project.ProjectManager as ProjectManager
import           Flowbox.Config.Config                (Config)
import           Flowbox.Prelude



type Error = String

type Batch a = (Functor m, MonadIO m) => EitherT Error (StateT BatchEnv m) a

data BatchEnv = BatchEnv { config         :: Config
                         , projectManager :: ProjectManager
                         } deriving (Show)



runBatch :: BatchEnv -> Batch a -> IO (Either Error a)
runBatch env batch = fst <$> runStateT (runEitherT batch) env

make :: Config -> BatchEnv
make config' = BatchEnv config' ProjectManager.empty


attributeKey :: String
attributeKey = "Batch-0.1"

