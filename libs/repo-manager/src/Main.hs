---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Main where

import Flowbox.Prelude

main = putStrLn "Hello World"


--module Flowbox.PackageManager.Data.Package where

--import Flowbox.Prelude
--import Data.Map (Map)


--type ID = Int

----TODO [PM] : Remove
--type Version      = [Int]
--type Architecture = String
--type URI          = String -- lokalne lub zdalne
--type Command      = String
----


--type PackageName = String
--type PackagePath = [String]


----data VersionDependency = Simple Ordering Version
----                       | And VersionDependency VersionDependency
----                       | Or  VersionDependency VersionDependency
----                       deriving (Show)


--data VersionRange = { max :: Maybe Version
--                    , min :: Maybe Version
--                    }

--data Constrain = Include VersionRange
--               | Exclude VersionRange
--               deriving (Show)



--data Dependency = Dependency { name'      :: PluginName 
--                             , constrains :: [Constrain]
--                             } deriving (Show)


--type SourceURI = Map Architecture URI


--data Package = Package { name            :: PluginName
--                     , path            :: PackagePath
--                     , version         :: Version
--                     , source          :: SourceURI
--                     , installScript   :: [Command]
--                     , uninstallScript :: [Command]
--                     , dependencies    :: [Dependency]
--                     } deriving (Show)


--data Category = Category Name [Category]
--              | Package_  Package   
--              deriving (Show)


---------------------------

--data World = World { installed :: [Package]
--                   , selected  :: [Package]
--                   }

--data PkgDb = PkgDb { packages :: [Package] }





---------
--searchby na regexpach
--biblioteka do gita
--git pull itd