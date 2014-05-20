---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.System.FilePath where

import           Flowbox.Prelude
import qualified Flowbox.System.UniPath as UniPath



normalise' :: FilePath -> FilePath
normalise' filePath = let
    norm = UniPath.toUnixString . UniPath.normalise . UniPath.fromUnixString
    in if last filePath == '/'
        then (norm filePath) ++ "/"
        else norm filePath
