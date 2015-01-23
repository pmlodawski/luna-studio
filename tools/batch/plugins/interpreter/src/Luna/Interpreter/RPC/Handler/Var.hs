---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Interpreter.RPC.Handler.Var where

import Data.IntSet as IntSet

import qualified Flowbox.Batch.Handler.Common            as Batch
import           Flowbox.Bus.RPC.RPC                     (RPC)
import           Flowbox.Control.Error                   hiding (err)
import           Flowbox.Prelude                         hiding (Context)
import           Flowbox.ProjectManager.Context          (Context)
import qualified Luna.Graph.Node                         as Node
import           Luna.Graph.Node.Expr                    (NodeExpr)
import qualified Luna.Graph.PropertyMap                  as PropertyMap
import qualified Luna.Graph.View.Default.DefaultsMap     as DefaultsMap
import           Luna.Interpreter.RPC.Handler.Lift
import qualified Luna.Interpreter.Session.Cache.Cache    as Cache
import           Luna.Interpreter.Session.Data.CallPoint (CallPoint (CallPoint))
import qualified Luna.Interpreter.Session.Env            as Env
import           Luna.Interpreter.Session.Memory.Manager (MemoryManager)
import           Luna.Interpreter.Session.Session        (SessionST)
import qualified Luna.Interpreter.Session.Var            as Var
import qualified Luna.Lib.Lib                            as Lib
import qualified Luna.Lib.Manager                        as LibManager
import qualified Luna.Pass.Analysis.ID.ExtractIDs        as ExtractIDs
import Control.Monad (forM_)


insertTimeRef :: Lib.ID -> Node.ID -> Node.ID
              -> NodeExpr -> RPC Context (SessionST mm) ()
insertTimeRef libraryID nodeID defID defExpr = do
    ids <- hoistEither =<< ExtractIDs.runNodeExpr defExpr
    liftSession $ do
        Env.insertDependentNode (CallPoint libraryID nodeID) defID
        Env.insertDependentNodes (CallPoint libraryID defID) ids
        forM_ (Var.timeRefIds defExpr) $ \ timeRefID ->
            Env.insertTimeRef (CallPoint libraryID timeRefID)


deleteTimeRef :: MemoryManager mm
              => Lib.ID -> Node.ID -> Node.ID
              -> NodeExpr -> RPC Context (SessionST mm) ()
deleteTimeRef libraryID nodeID defID defExpr = do
    ids <- hoistEither =<< ExtractIDs.runNodeExpr defExpr
    liftSession $ do
        Cache.deleteNode libraryID defID
        mapM_ (Cache.deleteNode libraryID) $ IntSet.toList ids
        Env.deleteDependentNode (CallPoint libraryID nodeID) defID
        forM_ (Var.timeRefIds defExpr) $ \ timeRefID ->
            Env.deleteTimeRef (CallPoint libraryID timeRefID)


rebuildTimeRefs :: RPC Context (SessionST mm) ()
rebuildTimeRefs = do
    activeProjectID <- liftSession Env.getProjectID
    libManager      <- Batch.getLibManager activeProjectID
    let libraries = LibManager.labNodes libManager
        procLibs (libraryID, library) = mapM (procDM libraryID) $ PropertyMap.getDefaultsMaps $ library ^. Lib.propertyMap
        procDM libraryID (nodeID, defaultsMap) = mapM_ (process libraryID nodeID) $ DefaultsMap.elems defaultsMap
        process libraryID nodeID (defID, defExpr) = insertTimeRef libraryID nodeID defID defExpr
    liftSession Env.cleanTimeRefs
    mapM_ procLibs libraries
