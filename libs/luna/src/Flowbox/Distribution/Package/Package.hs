---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Distribution.Package.Package where

import           Data.Aeson
import qualified Data.Aeson.TH as JSON
import           Data.Default  (Default, def)
import           GHC.Generics

import           Flowbox.Data.Version                    (Version)
import qualified Flowbox.Data.Version                    as Version
import           Flowbox.Distribution.License            (License)
import           Flowbox.Distribution.Package.Dependency (Dependency)
import qualified Flowbox.Distribution.Package.Dependency as Dependency
import           Flowbox.Prelude                         hiding (id)



data PackageId = PackageId { _name    :: String
                           , _version :: Version
                           } deriving (Show, Generic)
makeLenses (''PackageId)


data Package = Package { _id           :: PackageId
                       , _synopsis     :: String
                       , _description  :: String
                       , _homepage     :: String
                       , _url          :: String
                       , _bugReports   :: String
                       , _license      :: License
                       , _licenseFile  :: Maybe String
                       , _authors      :: [String]
                       , _maintainers  :: [String]
                       , _copyright    :: String
                       , _tags         :: [String]
                       , _dependencies :: [Dependency]
                       } deriving (Show, Generic)

makeLenses (''Package)


chooseNewer :: Package -> Package -> Package
chooseNewer pkg1 pkg2 = if (pkg1^.id^.version > pkg2^.id^.version) then pkg1 else pkg2

------------------------------------------------------------------------
-- INSTANCES
------------------------------------------------------------------------

instance Default Package where
    def = Package { _id           = def
                  , _synopsis     = def
                  , _description  = def
                  , _homepage     = def
                  , _url          = def
                  , _bugReports   = def
                  , _license      = def
                  , _licenseFile  = def
                  , _authors      = def
                  , _maintainers  = def
                  , _copyright    = def
                  , _tags         = def
                  , _dependencies = def
                  }

instance Default PackageId where
    def = PackageId { _name    = "unnamed"
                    , _version = def
                    }

JSON.deriveJSON JSON.defaultOptions{JSON.fieldLabelModifier = drop 1} ''Package
JSON.deriveJSON JSON.defaultOptions{JSON.fieldLabelModifier = drop 1} ''PackageId


