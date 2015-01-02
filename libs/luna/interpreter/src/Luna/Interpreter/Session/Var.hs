---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Luna.Interpreter.Session.Var where

import           Flowbox.Prelude
import           Luna.Interpreter.Session.Env     (Session)
import qualified Luna.Interpreter.Session.Session as Session


timeVar :: String
timeVar = "time"


timeRef :: String
timeRef = "Time#"


timeSet :: Float -> Session mm ()
timeSet = Session.runAssignment timeVar . show
