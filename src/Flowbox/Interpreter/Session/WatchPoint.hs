---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Interpreter.Session.WatchPoint where

import           Control.Monad.Trans.State
import           Flowbox.Data.MapTree                (MapForest)
import qualified Flowbox.Data.MapTree                as MapTree
import qualified Flowbox.Interpreter.Mockup.Node     as Node
import qualified Flowbox.Interpreter.Session.Env     as Env
import           Flowbox.Interpreter.Session.Session (Session)
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Interpreter.Session.WatchPoint"


add :: [Node.ID] -> Session ()
add point = modify (Env.watchPoints %~ MapTree.insert point)


delete :: [Node.ID] -> Session ()
delete point = modify (Env.watchPoints %~ MapTree.delete point)


all :: Session (MapForest Node.ID)
all = gets (view Env.watchPoints)
