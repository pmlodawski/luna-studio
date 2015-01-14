---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Luna.Interpreter.Session.Var where

import           Flowbox.Prelude
import           Luna.Graph.Node.Expr       (NodeExpr)
import qualified Luna.Graph.Node.Expr       as NodeExpr
import qualified Luna.Graph.Node.StringExpr as StringExpr



timeRef :: String
timeRef = "time"


timeNodeExpr :: NodeExpr
timeNodeExpr = NodeExpr.StringExpr
             $ StringExpr.fromString timeRef


isTimeRefNodeExpr :: NodeExpr -> Bool
isTimeRefNodeExpr = (==) timeNodeExpr
