---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Project.Project(
    Project(..),
        ID,
        empty,
        make,
) where

import           Flowbox.Luna.Data.Attributes (Attributes)
import qualified Flowbox.Luna.Data.Attributes as Attributes
import           Flowbox.Luna.Lib.LibManager  (LibManager)
import qualified Flowbox.Luna.Lib.LibManager  as LibManager
import           Flowbox.Prelude              hiding (empty)
import           Flowbox.System.UniPath       (UniPath)
import qualified Flowbox.System.UniPath       as UniPath



data Project = Project { name     :: String
                       , path     :: UniPath
                       , libPaths :: [UniPath]
                       , libs     :: LibManager
                       , attrs    :: Attributes
                       } deriving(Show)

type ID = Int


empty :: Project
empty = Project "" UniPath.empty [] LibManager.empty Attributes.empty


make :: String -> UniPath -> Project
make aname upath = empty { name = aname, path = upath }
