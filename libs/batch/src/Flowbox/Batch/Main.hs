---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

import qualified Flowbox.Batch.Project.Project        as Project
import           Flowbox.Batch.Project.Project          (Project)
import qualified Flowbox.Batch.Project.ProjectManager as ProjectManager
import           Flowbox.Batch.Project.ProjectManager   (ProjectManager)
import qualified Flowbox.System.UniPath               as UniPath
import           Flowbox.System.UniPath                 (UniPath)
import qualified Flowbox.Luna.Samples.HelloWorld      as HelloWorld


main :: IO()
main = do
	let
		pm = ProjectManager.empty
		p = Project.empty { Project.name = "TestProject"
						  , Project.path = UniPath.fromUnixString "samples/TestProject"
						  , Project.libs = HelloWorld.libman
		                  }

	ProjectManager.createProject p

	print p
	return ()