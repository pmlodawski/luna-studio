---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Luna.Interpreter.Session.Cache.Invalidate where

import           Control.Monad.State hiding (mapM, mapM_)
import qualified Data.List           as List

import qualified Flowbox.Data.MapForest                      as MapForest
import           Flowbox.Prelude                             hiding (matching)
import           Flowbox.System.Log.Logger
import           Luna.AST.Control.Crumb                      (Breadcrumbs)
import qualified Luna.AST.Control.Crumb                      as Crumb
import qualified Luna.Graph.Graph                            as Graph
import qualified Luna.Graph.Node                             as Node
import qualified Luna.Interpreter.Session.AST.Traverse       as Traverse
import qualified Luna.Interpreter.Session.Cache.Cache        as Cache
import           Luna.Interpreter.Session.Cache.Info         (CacheInfo)
import qualified Luna.Interpreter.Session.Cache.Info         as CacheInfo
import           Luna.Interpreter.Session.Cache.Status       (CacheStatus)
import qualified Luna.Interpreter.Session.Cache.Status       as CacheStatus
import           Luna.Interpreter.Session.Data.CallDataPath  (CallDataPath)
import qualified Luna.Interpreter.Session.Data.CallDataPath  as CallDataPath
import           Luna.Interpreter.Session.Data.CallPoint     (CallPoint (CallPoint))
import qualified Luna.Interpreter.Session.Data.CallPoint     as CallPoint
import           Luna.Interpreter.Session.Data.CallPointPath (CallPointPath)
import           Luna.Interpreter.Session.Data.DefPoint      (DefPoint (DefPoint))
import qualified Luna.Interpreter.Session.Env                as Env
import           Luna.Interpreter.Session.Session            (Session)
import qualified Luna.Interpreter.Session.TargetHS.Reload    as Reload
import qualified Luna.Lib.Lib                                as Library
import qualified Luna.Lib.Manager                            as LibManager



logger :: LoggerIO
logger = getLoggerIO $(moduleName)


modifyAll :: Session ()
modifyAll = do
    logger info "Mark modified: everything"
    modifyMatching $ const . const True
    libIDs <- LibManager.nodes <$> Env.getLibManager
    --Env.addReload (head libIDs) Reload.ReloadLibrary
    mapM_ (`Env.addReload` Reload.ReloadLibrary) libIDs


modifyLibrary :: Library.ID -> Session ()
modifyLibrary libraryID = do
    let matchLib k _ = last k ^. CallPoint.libraryID == libraryID
    logger info $ "Mark modified: library " ++ show libraryID
    modifyMatching matchLib
    Env.addReload libraryID Reload.ReloadLibrary


--modifyDef :: Library.ID -> AST.ID -> Session ()
--modifyDef libraryID defID = do
--    let matchDef k v = last k ^. CallPoint.libraryID == libraryID
--                         && v ^. CacheInfo.defID     == defID
--    logger info $ "Mark modified: definition " ++ show (libraryID, defID)
--    modifyMatching matchDef


modifyBreadcrumbsRec :: Library.ID -> Breadcrumbs -> Session ()
modifyBreadcrumbsRec libraryID bc = do
    let matchBC k v = last k ^. CallPoint.libraryID   == libraryID
                        && List.isPrefixOf bc (v ^. CacheInfo.breadcrumbs)
    logger info $ "Mark modified: breadcrumbs rec. " ++ show (libraryID, bc)
    modifyMatching matchBC
    Env.addReload libraryID Reload.ReloadLibrary


modifyBreadcrumbs :: Library.ID -> Breadcrumbs -> Session ()
modifyBreadcrumbs libraryID bc = do
    let matchBC k v = last k ^. CallPoint.libraryID == libraryID
                        && v ^. CacheInfo.breadcrumbs == bc
    logger info $ "Mark modified: breadcrumbs " ++ show (libraryID, bc)
    modifyMatching matchBC
    let lastBC = last bc
    Env.addReload libraryID $ if Crumb.isClass lastBC
        then Reload.mkReloadClasses bc
        else if Crumb.isFunction lastBC
            then Reload.ReloadFunctions
            else Reload.ReloadLibrary


modifyNode :: Library.ID -> Node.ID -> Session ()
modifyNode libraryID nodeID = do
    let matchNode k _ = last k == CallPoint libraryID nodeID
    logger info $ "Mark modified: node " ++ show (libraryID, nodeID)
    modifyMatching matchNode
    Env.addReload libraryID Reload.ReloadFunctions


modifyNodeSuccessors :: Library.ID -> Breadcrumbs -> Node.ID -> Session ()
modifyNodeSuccessors libraryID bc nodeID = do
    logger info $ concat ["Mark modified: node ", show (libraryID, nodeID), " successors"]
    graph <- fst <$> Env.getGraph (DefPoint libraryID bc)
    let successors = Graph.suc graph nodeID
    mapM_ (modifyNode libraryID) successors
    Env.addReload libraryID Reload.ReloadFunctions


modifyMatching :: (CallPointPath -> CacheInfo -> Bool) -> Session ()
modifyMatching predicate = do
    matching <- MapForest.find predicate <$> Cache.cached
    mapM_ (setParentsStatus CacheStatus.Modified . fst) matching
    Env.setAllReady False


setParentsStatus :: CacheStatus -> CallPointPath -> Session ()
setParentsStatus _      []            = return ()
setParentsStatus status callPointPath = do
    Cache.setStatus status callPointPath
    setParentsStatus status $ init callPointPath


markSuccessors :: CallDataPath -> CacheStatus -> Session ()
markSuccessors callDataPath status =
    Traverse.next callDataPath >>=
    mapM_ (setParentsStatus status . CallDataPath.toCallPointPath)

