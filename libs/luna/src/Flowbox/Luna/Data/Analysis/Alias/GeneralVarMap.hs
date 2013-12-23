---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Data.Analysis.Alias.GeneralVarMap where

import Flowbox.Prelude

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap



data GeneralVarMap = GeneralVarMap { varmap :: IntMap (Either String Int) }
                                   deriving (Show)


empty :: GeneralVarMap
empty = GeneralVarMap IntMap.empty

