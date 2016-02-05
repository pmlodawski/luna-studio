---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Luna.Interpreter.RPC.Handler.Cache where

import           Data.Int                                         (Int32)

import           Flowbox.Bus.RPC.RPC                              (RPC)
import           Flowbox.Data.Convert
import           Flowbox.Prelude                                  hiding (Context, error, op)
import           Flowbox.ProjectManager.Context                   (Context)
import           Flowbox.System.Log.Logger
import qualified Generated.Proto.Dep.Crumb.Breadcrumbs            as Gen
import           Luna.DEP.Data.Serialize.Proto.Conversion.Crumb   ()
import           Luna.DEP.Data.Serialize.Proto.Conversion.Library ()
import           Luna.Interpreter.RPC.Handler.Lift
import qualified Luna.Interpreter.Session.Cache.Cache             as Cache
import qualified Luna.Interpreter.Session.Cache.Invalidate        as Invalidate
import           Luna.Interpreter.Session.Data.Time               (Time)
import qualified Luna.Interpreter.Session.Env                     as Env
import qualified Luna.Interpreter.Session.Memory.GPU              as GPUMemory
import           Luna.Interpreter.Session.Memory.Manager          (MemoryManager)
import qualified Luna.Interpreter.Session.Memory.Manager          as Manager
import           Luna.Interpreter.Session.Session                 (Session, SessionST)



logger :: LoggerIO
logger = getLoggerIO $moduleName

--- helpers ---------------------------------------------------------------

interpreterDo :: Int32 -> Session mm () -> RPC Context (SessionST mm) ()
interpreterDo projectID = interpreterDo' projectID . liftSession


interpreterDo' :: Int32 -> RPC Context (SessionST mm) () -> RPC Context (SessionST mm) ()
interpreterDo' projectID op = do
    activeProjectID <- liftSession Env.getProjectID
    when (activeProjectID == decodeP projectID) op


deleteAll :: MemoryManager mm => Int32 -> RPC Context (SessionST mm) ()
deleteAll projectID = interpreterDo projectID $ do
    Cache.deleteAll
    Env.cleanEnv
    Manager.cleanIfNeeded
    GPUMemory.performGC


modifyAll :: MemoryManager mm => Int32 -> RPC Context (SessionST mm) ()
modifyAll projectID = interpreterDo projectID $ do
    Invalidate.modifyAll
    Manager.cleanIfNeeded
    GPUMemory.performGC


closeProject :: Int32 -> RPC Context (SessionST mm) ()
closeProject projectID = interpreterDo projectID $ Env.unsetProjectID >> Env.cleanEnv


modifyLibrary :: Int32 -> Int32 -> RPC Context (SessionST mm) ()
modifyLibrary projectID =
    interpreterDo projectID . Invalidate.modifyLibrary . decodeP


modifyBreadcrumbsRec :: Int32 -> Int32 -> Gen.Breadcrumbs -> RPC Context (SessionST mm) ()
modifyBreadcrumbsRec projectID libraryID tbc = do
    bc <- decodeE tbc
    interpreterDo projectID $ Invalidate.modifyBreadcrumbsRec (decodeP libraryID) bc


modifyBreadcrumbs :: Int32 -> Int32 -> Gen.Breadcrumbs -> RPC Context (SessionST mm) ()
modifyBreadcrumbs projectID libraryID tbc = do
    bc <- decodeE tbc
    interpreterDo projectID $ Invalidate.modifyBreadcrumbs (decodeP libraryID) bc


modifyNode :: Int32 -> Int32 -> Int32 -> RPC Context (SessionST mm) ()
modifyNode projectID libraryID nodeID =
    interpreterDo projectID $ Invalidate.modifyNode (decodeP libraryID) (decodeP nodeID)


modifyNodeSuccessors :: Int32 -> Int32 -> Gen.Breadcrumbs -> Int32 -> RPC Context (SessionST mm) ()
modifyNodeSuccessors projectID libraryID tbc nodeID = do
    bc <- decodeE tbc
    interpreterDo projectID $ Invalidate.modifyNodeSuccessors (decodeP libraryID) bc (decodeP nodeID)


deleteNode :: MemoryManager mm => Int32 -> Int32 -> Int32 -> RPC Context (SessionST mm) ()
deleteNode projectID libraryID nodeID =
    interpreterDo projectID $ do
        Cache.deleteNode (decodeP libraryID) (decodeP nodeID)
        Manager.cleanIfNeeded


setTimeVar :: Time -> RPC Context (SessionST mm) ()
setTimeVar time = liftSession $ do
    Env.setTimeVar time
    Invalidate.modifyTimeRefs
