---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Interpreter.Session.AST.Cache where

import Control.Monad.State hiding (mapM, mapM_)

import qualified Flowbox.Data.MapForest                         as MapForest
import qualified Flowbox.Interpreter.Session.AST.Traverse       as Traverse
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
logger = getLoggerIO "Flowbox.Interpreter.Session.AST.Cache"



dump :: CallPointPath -> Session ()
dump callPointPath = do
    let varName = CallPointPath.toVarName callPointPath
    logger debug $ "Dumping " ++ varName
    Session.runStmt $ "print " ++ varName


exists :: CallPointPath -> Session Bool
exists callPointPath = MapForest.member callPointPath <$> gets (view Env.cached)


put :: CallPointPath -> Session ()
put callPointPath = modify $ Env.cached %~ MapForest.insert callPointPath


delete :: CallPointPath -> Session ()
delete callPointPath = modify $ Env.cached %~ MapForest.delete callPointPath


invalidateForward :: CallDataPath -> Session ()
invalidateForward callDataPath = do
    let callPointPath = CallDataPath.toCallPointPath callDataPath
    into    <- Traverse.into callDataPath
    let next = Traverse.nextLocal callDataPath
    mapM_ invalidateForward $ next ++ into
    invalidateCache callPointPath


invalidateUpper :: CallDataPath -> Session ()
invalidateUpper [] = return ()
invalidateUpper callDataPath = do
    let upper = init callDataPath
    invalidateCache $ CallDataPath.toCallPointPath callDataPath
    invalidateUpper upper
    let next = Traverse.nextLocal callDataPath
    mapM_ invalidateForward next



invalidate :: CallPointPath -> Session ()
invalidate callPointPath = do
    main         <- Session.findMain
    callDataPath <- CallDataPath.fromCallPointPath callPointPath main
    invalidateForward callDataPath
    invalidateUpper   callDataPath



invalidateCache :: CallPointPath -> Session ()
invalidateCache callPointPath =
    whenM (exists callPointPath) $ do
        let varName       = CallPointPath.toVarName callPointPath
            expression    = varName ++ " <- return ()"
        logger debug $ "Invalidating " ++ varName
        delete callPointPath
        Session.runStmt expression
        logger trace =<< MapForest.draw <$> gets (view Env.cached)
