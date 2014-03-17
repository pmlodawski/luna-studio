---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.ProjectManager.Context where

import           Data.IORef (IORef)
import qualified Data.IORef as IORef

import           Flowbox.Batch.Batch   (Batch)
import qualified Flowbox.Batch.Batch   as Batch
import           Flowbox.Config.Config (Config)
import           Flowbox.Prelude       hiding (Context)



type Context = IORef Batch


mk :: Config -> IO Context
mk cfg = IORef.newIORef $ Batch.make cfg
