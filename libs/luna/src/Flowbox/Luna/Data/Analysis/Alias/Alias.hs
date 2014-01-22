---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Luna.Data.Analysis.Alias.Alias where

import Flowbox.Prelude

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap


-- TODO [PM]: split to separate files; rename


data AA     = AA  { varmap :: IntMap Int }
                  deriving (Show)


type AAGeneral = IntMap (Maybe Int)


empty :: AA
empty = AA IntMap.empty

