-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.System.IO.IO(
        writeFile
) where

import           Flowbox.System.UniPath (UniPath)
import qualified Flowbox.System.UniPath as UniPath
import           Prelude                (IO, String)
import qualified Prelude                as Prelude


writeFile :: UniPath -> String -> IO ()
writeFile path = Prelude.writeFile (UniPath.toUnixString path)
