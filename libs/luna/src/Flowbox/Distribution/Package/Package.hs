---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}

module Flowbox.Distribution.Package.Package where

import           Flowbox.Prelude                         hiding (id)          
import qualified Flowbox.Data.Version                    as Version
import           Flowbox.Data.Version                      (Version)
import qualified Flowbox.Distribution.Package.Dependency as Dependency
import           Flowbox.Distribution.Package.Dependency   (Dependency)

import           GHC.Generics                              
import           Data.Aeson                                
import           Data.Monoid                               (Monoid, mempty)

data Package = Package { id           :: PackageIdentifier
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

data PackageIdentifier = PackageIdentifier { name    :: String
                                           , version :: Version
                                           } deriving (Show, Generic)



-------------------------------------------------
-- INSTANCES
-------------------------------------------------

instance Monoid Package where
    mempty = Package { id = mempty
                     }

instance Monoid PackageIdentifier where
    mempty = PackageIdentifier { name    = "unnamed"
                               , version = mempty
                               }

instance ToJSON Package
instance FromJSON Package

instance ToJSON PackageIdentifier
instance FromJSON PackageIdentifier

