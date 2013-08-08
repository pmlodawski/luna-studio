---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Project.Project(
    Project(..),
    empty
) where

import qualified Flowbox.Luna.Core                        as Core
import           Flowbox.Luna.Core                          (Core(..))
import qualified Flowbox.Luna.Lib.LibManager              as LibManager
import           Flowbox.Luna.Lib.LibManager                (LibManager)
import qualified Flowbox.Luna.Lib.Library                 as Library
import           Flowbox.Luna.Lib.Library                   (Library(..))
import qualified Flowbox.Luna.Network.Attributes          as Attributes
import           Flowbox.Luna.Network.Attributes            (Attributes)
import qualified Flowbox.Luna.Network.Def.DefManager      as DefManager
import           Flowbox.Luna.Network.Def.DefManager        (DefManager)
import qualified Flowbox.Luna.Network.Def.Definition      as Definition
import           Flowbox.Luna.Network.Def.Definition        (Definition(..))
import qualified Flowbox.Luna.Network.Flags               as Flags
import qualified Flowbox.Luna.Network.Graph.Graph         as Graph
import qualified Flowbox.Luna.System.UniPath              as UniPath
import           Flowbox.Luna.System.UniPath                (UniPath)
import qualified Flowbox.Luna.Type.Type                   as Type


data Project = Project { core  :: Core
                       , name  :: String
                       , path  :: UniPath
                       , attrs :: Attributes
                       } deriving(Show)


empty :: Project
empty = Project Core.empty "" UniPath.empty Attributes.empty