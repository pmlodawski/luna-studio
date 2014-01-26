---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Batch.Project.Project where

import           Flowbox.Batch.Process.Map    (ProcessMap)
import qualified Flowbox.Batch.Process.Map    as ProcessMap
import           Flowbox.Luna.Data.Attributes (Attributes)
import qualified Flowbox.Luna.Data.Attributes as Attributes
import           Flowbox.Luna.Lib.LibManager  (LibManager)
import qualified Flowbox.Luna.Lib.LibManager  as LibManager
import           Flowbox.Prelude              hiding (empty)
import           Flowbox.System.UniPath       (UniPath)
import qualified Flowbox.System.UniPath       as UniPath



data Project = Project { name       :: String
                       , path       :: UniPath
                       , libPaths   :: [UniPath]
                       , libs       :: LibManager
                       , attrs      :: Attributes

                       , processMap :: ProcessMap
                       } deriving(Show)


type ID = Int


empty :: Project
empty = Project "" UniPath.empty [] LibManager.empty Attributes.empty ProcessMap.empty


make :: String -> UniPath -> Attributes -> Project
make name' path' attrs' = empty { name = name'
                                , path = path'
                                , attrs = attrs'
                                }
