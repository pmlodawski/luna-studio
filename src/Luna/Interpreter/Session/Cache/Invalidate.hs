---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Luna.Interpreter.Session.Cache.Invalidate where

import           Control.Monad.State hiding (mapM, mapM_)
import qualified Data.List           as List

import qualified Flowbox.Data.MapForest                      as MapForest
import           Flowbox.Prelude                             hiding (matching)
import           Flowbox.System.Log.Logger
import qualified Luna.AST.Common                             as AST
import           Luna.AST.Control.Crumb                      (Breadcrumbs)
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
import           Luna.Interpreter.Session.Session            (Session)
import qualified Luna.Interpreter.Session.Session            as Session
import qualified Luna.Lib.Lib                                as Library



logger :: LoggerIO
logger = getLoggerIO "Luna.Interpreter.Session.Cache.Invalidate"


modifyAll :: Session ()
modifyAll = modifyMatching $ const . const True


modifyLibrary :: Library.ID -> Session ()
modifyLibrary libraryID = modifyMatching matchLib where
    matchLib k _ = last k ^. CallPoint.libraryID == libraryID


modifyDef :: Library.ID -> AST.ID -> Session ()
modifyDef libraryID defID = modifyMatching matchDef where
    matchDef k v = last k ^. CallPoint.libraryID == libraryID
                     && v ^. CacheInfo.defID     == defID


modifyBreadcrumbsRec :: Library.ID -> Breadcrumbs -> Session ()
modifyBreadcrumbsRec libraryID bc = modifyMatching matchBC where
    matchBC k v = last k ^. CallPoint.libraryID   == libraryID
                    && List.isPrefixOf bc (v ^. CacheInfo.breadcrumbs)


modifyBreadcrumbs :: Library.ID -> Breadcrumbs -> Session ()
modifyBreadcrumbs libraryID bc = modifyMatching matchBC where
    matchBC k v = last k ^. CallPoint.libraryID == libraryID
                    && v ^. CacheInfo.breadcrumbs == bc


modifyNode :: Library.ID -> Node.ID -> Session ()
modifyNode libraryID nodeID = modifyMatching matchNode where
    matchNode k _ = last k == CallPoint libraryID nodeID


modifyMatching :: (CallPointPath -> CacheInfo -> Bool) -> Session ()
modifyMatching predicate = do
    matching <- MapForest.find predicate <$> Cache.cached
    mapM_ (setParentsStatus CacheStatus.Modified . fst) matching
    Session.setAllReady False


setParentsStatus :: CacheStatus -> CallPointPath -> Session ()
setParentsStatus _      []            = return ()
setParentsStatus status callPointPath = do
    Cache.setStatus status callPointPath
    setParentsStatus status $ init callPointPath


markSuccessors :: CallDataPath -> CacheStatus -> Session ()
markSuccessors callDataPath status =
    Traverse.next callDataPath >>=
    mapM_ (setParentsStatus status . CallDataPath.toCallPointPath)

