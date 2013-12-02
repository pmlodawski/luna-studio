---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Data.Lib.Package where

import           Flowbox.Prelude   
import           Distribution.Version   (Version)
import qualified Distribution.Version as Version

data Package = Package { name         :: String
                       , version      :: Version
                       , synopsis     :: String
                       , description  :: String
                       , homepage     :: String
                       , bugReports   :: String
                       , license      :: String
                       , licenseFile  :: String
                       , authors      :: [String]
                       , maintainers  :: [String]
                       , copyright    :: String
                       , tags         :: [String]
                       , dependencies :: [Dependency]
                       } deriving (Show)


data Dependency = Dependency { name    :: String
                             , version :: CVersion
                             } deriving (Show)

data CVersion = CVersion { version      :: Version 
                         , restrictions :: [Restriction]
                         } deriving (Show)

data Constrain = EQ Version
               | LT Version
               | LE Version
               | GT Version
               | GE Version
               deriving (Show)
