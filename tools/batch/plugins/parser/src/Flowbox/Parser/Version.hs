---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Parser.Version where

import qualified Data.Version as Version

import qualified Flowbox.Parser.Config as Config
import           Flowbox.Prelude



full :: Bool -> String
full = parser


parser :: Bool -> String
parser numeric = (if numeric then "" else "Flowbox parser version ") ++ Version.showVersion Config.version
