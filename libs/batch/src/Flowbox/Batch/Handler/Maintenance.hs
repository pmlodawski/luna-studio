---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Handler.Maintenance where


import           Flowbox.Prelude                   
import qualified Flowbox.Batch.Batch             as Batch
import           Flowbox.Batch.Batch               (Batch)
import qualified Flowbox.Initializer.Initializer as Initializer



initialize :: Batch -> IO ()
initialize batch = do 
    Initializer.initializeIfNeeded $ Batch.config batch
