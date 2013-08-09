---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Project.Project(
    Project(..),
	ID,
	empty
) where

import qualified Flowbox.Luna.Core                        as Core
import           Flowbox.Luna.Core                          (Core(..))
import qualified Flowbox.Luna.Network.Attributes          as Attributes
import           Flowbox.Luna.Network.Attributes            (Attributes)
import qualified Flowbox.System.UniPath                   as UniPath
import           Flowbox.System.UniPath                     (UniPath)



data Project = Project { name  :: String
                       , path  :: UniPath
                       , core  :: Core
                       , attrs :: Attributes
                       } deriving(Show)

type ID = Int


empty :: Project
empty = Project "" UniPath.empty Core.empty Attributes.empty