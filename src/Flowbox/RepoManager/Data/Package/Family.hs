---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.RepoManager.Data.Package.Family where

import Data.Map (Map)

import           Flowbox.Prelude
import           Flowbox.RepoManager.Data.Package.Package    (Package)
import           Flowbox.RepoManager.Data.Version (Version)

type Family a = Map Version a
type NamedFamily a = (String, a)
type AvailableFamilies = Family Package

type Hash = String
type InstalledFamilies = Family (Map Hash Package)
