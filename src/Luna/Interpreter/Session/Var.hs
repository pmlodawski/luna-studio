---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Luna.Interpreter.Session.Var where

import           Flowbox.Prelude
import           Luna.Graph.Node.StringExpr       (StringExpr)
import qualified Luna.Graph.Node.StringExpr       as StringExpr
import           Luna.Interpreter.Session.Env     (Session)
import qualified Luna.Interpreter.Session.Session as Session


timeVar :: String
timeVar = "time"


timeRef :: String
timeRef = "Time#"


timeSet :: Float -> Session mm ()
timeSet time = do
    Session.runAssignment timeVar $ show time
