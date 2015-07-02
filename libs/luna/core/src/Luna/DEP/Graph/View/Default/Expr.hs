---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.DEP.Graph.View.Default.Expr where

import           Flowbox.Prelude
import qualified Luna.DEP.Graph.Node      as Node
import           Luna.DEP.Graph.Node.Expr (NodeExpr)


data DefaultExpr = DefaultExpr { _nodeID   :: Node.ID
                               , _nodeExpr :: NodeExpr
                               } deriving (Show, Eq, Read)

makeLenses ''DefaultExpr
