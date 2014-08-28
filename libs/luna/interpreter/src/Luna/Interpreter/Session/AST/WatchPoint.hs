---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Luna.Interpreter.Session.AST.WatchPoint where

import Control.Monad.State

import           Flowbox.Data.SetForest                      (SetForest)
import qualified Flowbox.Data.SetForest                      as SetForest
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import           Luna.Interpreter.Session.Data.CallPoint     (CallPoint)
import           Luna.Interpreter.Session.Data.CallPointPath (CallPointPath)
import qualified Luna.Interpreter.Session.Env                as Env
import           Luna.Interpreter.Session.Session            (Session)



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Interpreter.Session.AST.WatchPoint"


add :: CallPointPath -> Session ()
add callPath = modify (Env.watchPoints %~ SetForest.insert callPath)


delete :: CallPointPath -> Session ()
delete callPath = modify (Env.watchPoints %~ SetForest.delete callPath)


all :: Session (SetForest CallPoint)
all = gets (view Env.watchPoints)
