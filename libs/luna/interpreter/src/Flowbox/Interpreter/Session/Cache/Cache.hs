---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Interpreter.Session.Cache.Cache where

import Control.Monad.State hiding (mapM, mapM_)

import qualified Flowbox.Data.MapForest                         as MapForest
import           Flowbox.Interpreter.Session.Data.CacheInfo     (CacheInfo (CacheInfo))
import qualified Flowbox.Interpreter.Session.Data.CallData      as CallData
import           Flowbox.Interpreter.Session.Data.CallDataPath  (CallDataPath)
import qualified Flowbox.Interpreter.Session.Data.CallDataPath  as CallDataPath
import           Flowbox.Interpreter.Session.Data.CallPointPath (CallPointPath)
import qualified Flowbox.Interpreter.Session.Data.CallPointPath as CallPointPath
import qualified Flowbox.Interpreter.Session.Env                as Env
import           Flowbox.Interpreter.Session.Session            (Session)
import qualified Flowbox.Interpreter.Session.Session            as Session
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Interpreter.Session.Cache.Cache"


dump :: CallPointPath -> Session ()
dump callPointPath = do
    let varName = CallPointPath.toVarName callPointPath
    logger debug $ "Dumping " ++ varName
    Session.runStmt $ "print " ++ varName


exists :: CallPointPath -> Session Bool
exists callPointPath = MapForest.contains callPointPath <$> gets (view Env.cached)


put :: CallDataPath -> Session ()
put callDataPath = do
    let callPointPath = CallDataPath.toCallPointPath callDataPath
        cacheInfo     = CacheInfo $ last callDataPath ^. CallData.parentDefID
    modify $ Env.cached %~ MapForest.insert callPointPath cacheInfo


delete :: CallPointPath -> Session ()
delete callPointPath = modify $ Env.cached %~ MapForest.delete callPointPath
