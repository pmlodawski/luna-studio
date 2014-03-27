---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.RepoManager.Data.Repository where

import Flowbox.Prelude
import Flowbox.RepoManager.Data.Item.Item (Item)



data Repository = Repository { items :: [Item]
                             } deriving (Show)


data World = World { installed :: [Item]
                   , selected  :: [Item]
                   } deriving (Show)