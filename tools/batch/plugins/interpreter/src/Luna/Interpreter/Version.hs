---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Interpreter.Version where

import qualified Data.Version as Version

import           Flowbox.Prelude
import qualified Luna.Interpreter.Config as Config



full :: Bool -> String
full = interpreter


interpreter :: Bool -> String
interpreter numeric = (if numeric then "" else "Luna interpreter version ") ++ Version.showVersion Config.version
