---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Distribution.Package.PackageFamily where

import           GHC.Generics                              
import           Data.Aeson                                
import qualified Data.Aeson.TH                           as JSON
import           Data.Default                              (Default, def)

import           Flowbox.Prelude                         hiding (id)
import qualified Flowbox.Data.Version                    as Version
import           Flowbox.Data.Version                      (Version)
import qualified Flowbox.Distribution.Package.Dependency as Dependency
import           Flowbox.Distribution.Package.Dependency   (Dependency)
import           Flowbox.Distribution.License              (License)
import qualified Flowbox.Distribution.Package.Package    as Package
import           Flowbox.Distribution.Package.Package      (Package)




data PackageFamily = PackageFamily { _name              :: String
                                   , _availableVersions :: [Version]
                                   , _installedVersions :: [Version]
                                   , _synopsis          :: String
                                   , _description       :: String
                                   , _homepage          :: String
                                   , _url               :: String
                                   , _bugReports        :: String
                                   , _license           :: License
                                   , _licenseFile       :: Maybe String
                                   , _authors           :: [String]
                                   , _maintainers       :: [String]
                                   , _copyright         :: String
                                   , _tags              :: [String]
                                   , _dependencies      :: [Dependency]
                                   } deriving (Show, Generic)

makeLenses (''PackageFamily)


mk :: [Package] -> PackageFamily
mk = foldr update def


update :: Package -> PackageFamily -> PackageFamily
update pkg pkgF = pkgF & name .~  (pkg ^. Package.id ^. Package.name)

------------------------------------------------------------------------
-- INSTANCES
------------------------------------------------------------------------

instance Default PackageFamily where
    def = PackageFamily { _name              = def
                        , _availableVersions = def
                        , _installedVersions = def
                        , _synopsis          = def
                        , _description       = def
                        , _homepage          = def
                        , _url               = def
                        , _bugReports        = def
                        , _license           = def
                        , _licenseFile       = def
                        , _authors           = def
                        , _maintainers       = def
                        , _copyright         = def
                        , _tags              = def
                        , _dependencies      = def
                        }


JSON.deriveJSON JSON.defaultOptions{JSON.fieldLabelModifier = drop 1} ''PackageFamily


