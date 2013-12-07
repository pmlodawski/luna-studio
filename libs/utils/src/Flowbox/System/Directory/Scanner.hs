---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.System.Directory.Scanner where

import Control.Applicative

import           Flowbox.Prelude
import qualified Flowbox.System.Directory.Directory as Directory
import           Flowbox.System.UniPath             (UniPath)
import qualified Flowbox.System.UniPath             as UniPath



scan :: UniPath -> (UniPath -> Bool) -> IO [UniPath]
scan rootPath predicate = filter predicate <$> Directory.getDirectoryRecursive rootPath


scanByExts :: UniPath -> String -> IO [UniPath]
scanByExts rootPath extension = scan rootPath hasExtension where
    hasExtension path = (UniPath.extension path) == extension
