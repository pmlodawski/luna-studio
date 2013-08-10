-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.System.IO(
	writeFile
) where

import qualified Prelude                as Prelude
import           Prelude                  (String, IO)
import qualified Flowbox.System.UniPath as UniPath
import           Flowbox.System.UniPath   (UniPath)


writeFile :: UniPath -> String -> IO ()
writeFile path = Prelude.writeFile (UniPath.toUnixString path)
