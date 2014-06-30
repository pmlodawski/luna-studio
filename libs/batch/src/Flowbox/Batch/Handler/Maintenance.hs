---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Batch.Handler.Maintenance where


import           Flowbox.Batch.Batch             (Batch, get, liftIO)
import qualified Flowbox.Batch.Batch             as Batch
import qualified Flowbox.Initializer.Initializer as Initializer
import           Flowbox.Prelude



initialize :: Batch ()
initialize = do config <- view Batch.config <$> get
                liftIO $ Initializer.initializeIfNeeded config

