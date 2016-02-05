---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Syntax.Graph.DefaultsMap (
    module Luna.Syntax.Graph.DefaultsMap,
    module X,
) where

import           Data.Map               as X

import           Luna.Syntax.Expr       (LExpr)
import           Luna.Syntax.Graph.Port (DstPort)



type DefaultsMap a v = Map DstPort (LExpr a v)
