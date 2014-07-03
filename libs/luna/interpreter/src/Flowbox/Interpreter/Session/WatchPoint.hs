---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Interpreter.Session.WatchPoint where

import           Control.Monad.State
import           Flowbox.Data.MapForest                         (MapForest)
import qualified Flowbox.Data.MapForest                         as MapForest
import           Flowbox.Interpreter.Session.Data.CallPoint     (CallPoint)
import           Flowbox.Interpreter.Session.Data.CallPointPath (CallPointPath)
import qualified Flowbox.Interpreter.Session.Env                as Env
import           Flowbox.Interpreter.Session.Session            (Session)
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Interpreter.Session.WatchPoint"


add :: CallPointPath -> Session ()
add callPath = modify (Env.watchPoints %~ MapForest.insert callPath)


delete :: CallPointPath -> Session ()
delete callPath = modify (Env.watchPoints %~ MapForest.delete callPath)


all :: Session (MapForest CallPoint)
all = gets (view Env.watchPoints)
