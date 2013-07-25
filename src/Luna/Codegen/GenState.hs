---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Codegen.GenState(
    GenState(..)
) where

import           Luna.Network.Graph.Graph          (Graph)

data GenState = GenState {graph :: Graph} deriving (Show)