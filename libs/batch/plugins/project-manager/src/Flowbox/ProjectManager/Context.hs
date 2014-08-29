---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}

module Flowbox.ProjectManager.Context where

import           Flowbox.Batch.Batch   (BatchEnv)
import qualified Flowbox.Batch.Batch   as Batch
import           Flowbox.Config.Config (Config)



type Context = BatchEnv


mk :: Config -> Context
mk = Batch.make
