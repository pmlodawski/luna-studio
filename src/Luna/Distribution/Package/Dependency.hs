---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}

module Flowbox.Distribution.Package.Dependency where

import Data.Aeson
--import           Flowbox.Data.Version (Version)
--import qualified Flowbox.Data.Version as Version
import Flowbox.Prelude
import GHC.Generics

data Dependency = Dependency { name :: String
                             --, version :: CVersion
                             } deriving (Show, Generic, Ord, Eq)

------------------------------------------------------------------------
-- INSTANCES
------------------------------------------------------------------------

instance ToJSON Dependency
instance FromJSON Dependency
