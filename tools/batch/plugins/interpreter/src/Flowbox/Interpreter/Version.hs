---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Interpreter.Version where

import qualified Data.Version as Version

import qualified Flowbox.Interpreter.Config as Config
import           Flowbox.Prelude



full :: Bool -> String
full = interpreter


interpreter :: Bool -> String
interpreter numeric = (if numeric then "" else "Flowbox interpreter version ") ++ Version.showVersion Config.version
