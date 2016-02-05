---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.DEP.Graph.View.Default.DefaultsMap (
    module Luna.DEP.Graph.View.Default.DefaultsMap,
    module X,
) where

import           Data.Map                           as X

import qualified Luna.DEP.Graph.Node                as Node
import           Luna.DEP.Graph.Node.Expr           (NodeExpr)
import           Luna.DEP.Graph.View.PortDescriptor (PortDescriptor)



type DefaultsMap = Map PortDescriptor (Node.ID, NodeExpr)
