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
import           Flowbox.Interpreter.Session.Cache.Info         (CacheInfo)
import qualified Flowbox.Interpreter.Session.Cache.Info         as CacheInfo
import           Flowbox.Interpreter.Session.Cache.Status       (CacheStatus)
import qualified Flowbox.Interpreter.Session.Cache.Status       as CacheStatus
import           Flowbox.Interpreter.Session.Data.CallDataPath  (CallDataPath)
import qualified Flowbox.Interpreter.Session.Data.CallDataPath  as CallDataPath
import           Flowbox.Interpreter.Session.Data.CallPoint     (CallPoint (CallPoint))
import qualified Flowbox.Interpreter.Session.Data.CallPoint     as CallPoint
import           Flowbox.Interpreter.Session.Data.CallPointPath (CallPointPath)
import           Flowbox.Interpreter.Session.Session            (Session)
import qualified Flowbox.Luna.Data.AST.Common                   as AST
import qualified Flowbox.Luna.Data.Graph.Node                   as Node
import qualified Flowbox.Luna.Lib.Library                       as Library
import           Flowbox.Prelude                                hiding (matching)
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Interpreter.Session.Cache.Invalidate"


modifyDef :: Library.ID -> AST.ID -> Session ()
modifyDef libraryID defID = modifyMatching matchDef where
    matchDef k v = last k ^. CallPoint.libraryID == libraryID
                     && v ^. CacheInfo.defID == defID


modifyNode :: Library.ID -> Node.ID -> Session ()
modifyNode libraryID nodeID = modifyMatching matchNode where
    matchNode k _ = last k == CallPoint libraryID nodeID


modifyMatching :: (CallPointPath -> CacheInfo -> Bool) -> Session ()
modifyMatching predicate = do
    matching <- MapForest.find predicate <$> Cache.cached
    mapM_ (modifyParents . fst) matching


modifyParents :: CallPointPath -> Session ()
modifyParents [] = return ()
modifyParents callPointPath = do
    Cache.setStatus CacheStatus.Modified callPointPath
    modifyParents $ init callPointPath


markSuccessors :: CallDataPath -> CacheStatus -> Session ()
markSuccessors callDataPath status =
    Traverse.next callDataPath >>=
    mapM_ (Cache.setStatus status . CallDataPath.toCallPointPath)

