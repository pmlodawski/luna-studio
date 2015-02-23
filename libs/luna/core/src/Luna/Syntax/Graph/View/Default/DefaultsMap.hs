---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Syntax.Graph.View.Default.DefaultsMap (
    module Luna.Syntax.Graph.View.Default.DefaultsMap,
    module X,
) where

import Data.Map as X

import qualified Luna.Syntax.Graph.Node                as Node
import           Luna.Syntax.Graph.Node.Expr           (NodeExpr)
import           Luna.Syntax.Graph.View.PortDescriptor (PortDescriptor)



type DefaultsMap a e = Map PortDescriptor (Node.ID, NodeExpr a e)
