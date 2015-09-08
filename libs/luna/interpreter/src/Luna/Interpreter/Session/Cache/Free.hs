---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------

module Luna.Interpreter.Session.Cache.Free where

import           Flowbox.Prelude
import           Luna.Interpreter.Session.Data.KeyName      (KeyName)
import qualified Luna.Interpreter.Session.Env               as Env
import           Luna.Interpreter.Session.Session           (Session)
import qualified Luna.Interpreter.Session.TargetHS.Bindings as Bindings



freeKeyName :: KeyName -> Session mm ()
freeKeyName keyName = do
    keyNameStr <- Env.keyNameToString keyName
    lift2 $ Bindings.remove keyNameStr
