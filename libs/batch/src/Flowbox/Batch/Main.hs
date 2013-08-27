---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

import qualified Flowbox.Batch.Project.Project        as Project
import           Flowbox.Batch.Project.Project          (Project)
import qualified Flowbox.Batch.Project.ProjectManager as ProjectManager
import           Flowbox.Batch.Project.ProjectManager   (ProjectManager)
import qualified Flowbox.System.UniPath               as UniPath
import           Flowbox.System.UniPath                 (UniPath)
import qualified Flowbox.Luna.Samples.HelloWorld      as HelloWorld
import qualified Flowbox.Batch.Samples.Modules        as Sample


main :: IO()
main = do
	print "helpme world"
