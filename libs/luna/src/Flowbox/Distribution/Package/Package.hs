---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}

module Flowbox.Distribution.Package.Package where

import           Flowbox.Prelude   
import qualified Flowbox.Data.Version                    as Version
import           Flowbox.Data.Version                      (Version)
import qualified Flowbox.Distribution.Package.Dependency as Dependency
import           Flowbox.Distribution.Package.Dependency   (Dependency)

import           GHC.Generics
import           Data.Aeson

data Package = Package { name         :: String
                       --, version      :: Version
                       --, synopsis     :: String
                       --, description  :: String
                       --, homepage     :: String
                       --, bugReports   :: String
                       --, license      :: String
                       --, licenseFile  :: String
                       --, authors      :: [String]
                       --, maintainers  :: [String]
                       --, copyright    :: String
                       --, tags         :: [String]
                       --, dependencies :: [Dependency]
                       } deriving (Show, Generic)


