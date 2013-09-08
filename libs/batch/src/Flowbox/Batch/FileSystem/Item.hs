---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.FileSystem.Item (
    Item(..)
) where 

import           Flowbox.Prelude          
import           Flowbox.System.UniPath   (UniPath)



data Item = File      { path :: UniPath, size :: Int }
          | Directory { path :: UniPath, size :: Int }
          | Other     { path :: UniPath, size :: Int }