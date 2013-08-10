-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.System.Directory(
	createDirectoryIfMissing
) where


import qualified System.Directory         
import qualified Flowbox.System.UniPath as UniPath
import           Flowbox.System.UniPath   (UniPath)

createDirectoryIfMissing :: Bool -> UniPath -> IO ()
createDirectoryIfMissing create_parents path = System.Directory.createDirectoryIfMissing create_parents (UniPath.toUnixString path)
