---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}

module Flowbox.Distribution.Package.Serialization where

import           Flowbox.Distribution.Package.Dependency (Dependency)
import           Flowbox.Distribution.Package.Package    (Package)
import           Flowbox.Data.Version                    (Version, Stage)
import           Data.Aeson

instance ToJSON Dependency
instance ToJSON Package
instance ToJSON Version
instance ToJSON Stage


