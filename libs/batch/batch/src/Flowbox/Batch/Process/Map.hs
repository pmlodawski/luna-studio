---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Batch.Process.Map (
    module Data.Map,
    ID,
    ProcessMap,
) where


import Data.Map

import Flowbox.Batch.Process.Handle (Handle)
import Flowbox.Prelude



type ProcessMap = Map ID Handle


type ID = Int
