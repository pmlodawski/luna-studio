---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Data.AliasAnalysis where

import Flowbox.Prelude

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap



data AA     = AA  { varmap :: IntMap Int }
                  deriving (Show)


empty :: AA
empty = AA IntMap.empty
