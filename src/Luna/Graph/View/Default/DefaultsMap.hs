---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Graph.View.Default.DefaultsMap (
    module Luna.Graph.View.Default.DefaultsMap,
    module X,
) where

import Data.Map as X

import qualified Luna.Graph.Node                as Node
import           Luna.Graph.View.Default.Value  (Value)
import           Luna.Graph.View.PortDescriptor (PortDescriptor)



type DefaultsMap = Map PortDescriptor (Node.ID, Value)
