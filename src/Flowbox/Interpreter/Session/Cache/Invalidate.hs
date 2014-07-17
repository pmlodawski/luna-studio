---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Interpreter.Session.Cache.Invalidate where

import Control.Monad.State hiding (mapM, mapM_)

import qualified Flowbox.Data.MapForest                         as MapForest
import qualified Flowbox.Interpreter.Session.AST.Traverse       as Traverse
import qualified Flowbox.Interpreter.Session.Cache.Cache        as Cache
import           Flowbox.Interpreter.Session.Data.CacheInfo     (CacheInfo)
import qualified Flowbox.Interpreter.Session.Data.CacheInfo     as CacheInfo
import qualified Flowbox.Interpreter.Session.Data.CallData      as CallData
import           Flowbox.Interpreter.Session.Data.CallDataPath  (CallDataPath)
import qualified Flowbox.Interpreter.Session.Data.CallDataPath  as CallDataPath
import           Flowbox.Interpreter.Session.Data.CallPoint     (CallPoint (CallPoint))
import qualified Flowbox.Interpreter.Session.Data.CallPoint     as CallPoint
import           Flowbox.Interpreter.Session.Data.CallPointPath (CallPointPath)
import qualified Flowbox.Interpreter.Session.Data.CallPointPath as CallPointPath
import qualified Flowbox.Interpreter.Session.Env                as Env
import           Flowbox.Interpreter.Session.Session            (Session)
import qualified Flowbox.Interpreter.Session.Session            as Session
import qualified Flowbox.Luna.Data.AST.Common                   as AST
import qualified Flowbox.Luna.Data.Graph.Node                   as Node
import qualified Flowbox.Luna.Lib.Library                       as Library
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Interpreter.Session.Cache.Invalidate"


invalidateDef :: Library.ID -> AST.ID -> Session ()
invalidateDef libraryID defID = invalidateMatching matchDef where
    matchDef k v = last k ^. CallPoint.libraryID == libraryID
                     && v ^. CacheInfo.defID == defID


invalidateNode :: Library.ID -> Node.ID -> Session ()
invalidateNode libraryID nodeID = invalidateMatching matchNode where
    matchNode k _ = last k == CallPoint libraryID nodeID


invalidateMatching :: (CallPointPath -> CacheInfo -> Bool) -> Session ()
invalidateMatching predicate = do
    matching <- MapForest.find predicate <$> Cache.cached
    mapM_ (invalidate . fst) matching


invalidate :: CallPointPath -> Session ()
invalidate callPointPath = do
    main         <- Session.findMain
    callDataPath <- CallDataPath.fromCallPointPath callPointPath main
    invalidate' callDataPath


invalidate' :: CallDataPath -> Session ()
invalidate' callDataPath = do
    invalidateCache' callDataPath
    let node = last callDataPath ^. CallData.node
    case node of
        Node.Outputs -> do let upper = Traverse.up callDataPath
                               next  = Traverse.nextLocal upper
                           invalidateCache' upper
                           mapM_ invalidate' next
        _            -> do into <- Traverse.into callDataPath
                           mapM_ invalidateInside into
                           let next = Traverse.nextLocal callDataPath
                           mapM_ invalidate' next


invalidateInside :: CallDataPath -> Session ()
invalidateInside callDataPath = do
    let callPointPath = CallDataPath.toCallPointPath callDataPath
    into    <- Traverse.into callDataPath
    let next = Traverse.nextLocal callDataPath
    mapM_ invalidateInside $ next ++ into
    invalidateCache callPointPath


invalidateCache :: CallPointPath -> Session ()
invalidateCache callPointPath =
    whenM (Cache.exists callPointPath) $ do
        let varName       = CallPointPath.toVarName callPointPath
            expression    = varName ++ " <- return ()"
        logger debug $ "Invalidating " ++ varName
        Cache.delete callPointPath
        Session.runStmt expression
        logger trace =<< MapForest.draw <$> gets (view Env.cached)


invalidateCache' :: CallDataPath -> Session ()
invalidateCache' = invalidateCache . CallDataPath.toCallPointPath
