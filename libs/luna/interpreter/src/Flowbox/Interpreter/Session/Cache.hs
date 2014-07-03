---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Interpreter.Session.Cache where

import Control.Monad.State hiding (mapM, mapM_)

import qualified Flowbox.Data.MapForest                         as MapForest
import           Flowbox.Interpreter.Session.Data.CallPointPath (CallPointPath)
import qualified Flowbox.Interpreter.Session.Env                as Env
import           Flowbox.Interpreter.Session.Session            (Session)
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Interpreter.Session.Cache"



--dump :: CallPath -> Session ()
--dump callPath = do
--    logger debug $ "Dumping " ++ show callPath
--    Session.runStmt ("print " ++ argPrefix ++ show callPath)


exists :: CallPointPath -> Session Bool
exists callPointPath = MapForest.member callPointPath <$> gets (view Env.cached)


put :: CallPointPath -> Session ()
put callPointPath = modify $ Env.cached %~ MapForest.insert callPointPath


-- TODO [PM] delete chilren from GhcMonad
--delete :: CallPointPath -> Session ()
--delete callPointPath = modify $ Env.cached %~ MapForest.delete callPointPath
