---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Interpreter.RPC.Handler.Lift where

import Flowbox.Bus.RPC.RPC              (RPC)
import Flowbox.Control.Error
import Flowbox.Prelude                  hiding (Context)
import Flowbox.ProjectManager.Context   (Context)
import Luna.Interpreter.Session.Session (Session, SessionST)



liftSession :: Session a -> RPC Context SessionST a
liftSession a = hoistEither =<< lift2 (runEitherT a)
