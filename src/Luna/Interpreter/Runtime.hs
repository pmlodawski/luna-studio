--------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2015
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------

module Luna.Interpreter.Runtime (
    module Luna.Interpreter.Runtime,
    module X
) where

import           Luna.Interpreter.Runtime.Hash as X
import           Luna.Interpreter.Runtime.HMap as X (HKey, HMap, T)
import qualified Luna.Interpreter.Runtime.HMap as HMap
import           Prelude



hmapCreateKeyWithWitness :: a -> IO (HKey T a)
hmapCreateKeyWithWitness = const HMap.createKey

hmapCreateKey = HMap.createKey
hmapInsert    = HMap.insert
hmapLookup    = HMap.lookup
hmapGet       = HMap.findWithDefault (error "Luna.Interpreter.Runtime.hmapGet: Could not find item in HMap")
