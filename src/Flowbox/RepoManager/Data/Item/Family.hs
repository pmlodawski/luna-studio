---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.RepoManager.Data.Item.Family where

import Data.Map (Map)

import           Flowbox.Prelude
import           Flowbox.RepoManager.Data.Item.Item    (Item)
import qualified Flowbox.RepoManager.Data.Item.Name    as Item
import           Flowbox.RepoManager.Data.Item.Version (Version)



type Family a = Map Version a


type NamedFamily a = (Item.Name, a)


type AvailableFamilies = Family Item


type Hash = String
type InstalledFamilies = Family (Map Hash Item)
