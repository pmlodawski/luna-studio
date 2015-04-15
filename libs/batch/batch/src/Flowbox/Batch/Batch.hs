---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Batch.Batch (
  module X
, module Flowbox.Batch.Batch
) where

import           Control.Monad.State        as X
import           Control.Monad.Trans.Either as X
import           Data.Bimap                 (Bimap)
import qualified Data.Bimap                 as Bimap
import           Data.Int                   (Int32)

import           Flowbox.Batch.Project.ProjectManager (ProjectManager)
import qualified Flowbox.Batch.Project.ProjectManager as ProjectManager
import           Flowbox.Config.Config                (Config)
import           Flowbox.Prelude



type Error = String


type IDMap = Bimap Int Int

emptyIDMap :: IDMap
emptyIDMap = Bimap.empty


type Batch a = (Functor m, MonadIO m) => EitherT Error (StateT BatchEnv m) a

data BatchEnv = BatchEnv { _config         :: Config
                         , _projectManager :: ProjectManager
                         , _updateNo       :: Int32
                         , _idMap          :: IDMap
                         } deriving (Show)


makeLenses ''BatchEnv


runBatch :: BatchEnv -> Batch a -> IO (Either Error a)
runBatch env batch = fst <$> runStateT (runEitherT batch) env


make :: Config -> BatchEnv
make config' = BatchEnv config' ProjectManager.empty 0 emptyIDMap


attributeKey :: String
attributeKey = "Batch-0.1"
