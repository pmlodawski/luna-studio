---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Luna.Interpreter.RPC.Handler.Cache where

import Data.Int (Int32)

import           Flowbox.Batch.Tools.Serialize.Proto.Conversion.Project ()
import           Flowbox.Bus.RPC.RPC                                    (RPC)
import           Flowbox.Prelude                                        hiding (Context, error, op)
import           Flowbox.ProjectManager.Context                         (Context)
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Crumb.Breadcrumbs                      as Gen
import           Luna.Interpreter.Proto.CallPointPath                   ()
import           Luna.Interpreter.RPC.Handler.Lift
import qualified Luna.Interpreter.Session.Cache.Cache                   as Cache
import qualified Luna.Interpreter.Session.Cache.Invalidate              as Invalidate
import           Luna.Interpreter.Session.Data.CallPoint                (CallPoint (CallPoint))
import qualified Luna.Interpreter.Session.Env                           as Env
import           Luna.Interpreter.Session.Memory.Manager                (MemoryManager)
import qualified Luna.Interpreter.Session.Memory.Manager                as Manager
import           Luna.Interpreter.Session.Session                       (Session, SessionST)
import qualified         Luna.Interpreter.Session.Memory.GPU as GPUMemory



logger :: LoggerIO
logger = getLoggerIO $(moduleName)

--- helpers ---------------------------------------------------------------

interpreterDo :: Int32 -> Session mm () -> RPC Context (SessionST mm) ()
interpreterDo projectID op = do
    activeProjectID <- liftSession Env.getProjectID
    when (activeProjectID == decodeP projectID) $ liftSession op


deleteAll :: MemoryManager mm => Int32 -> RPC Context (SessionST mm) ()
deleteAll projectID = interpreterDo projectID $ do
    Cache.deleteAll
    Manager.cleanIfNeeded
    GPUMemory.performGC


modifyAll :: MemoryManager mm => Int32 -> RPC Context (SessionST mm) ()
modifyAll projectID = interpreterDo projectID $ do
    Invalidate.modifyAll
    Manager.cleanIfNeeded
    GPUMemory.performGC


closeProject :: Int32 -> RPC Context (SessionST mm) ()
closeProject projectID = interpreterDo projectID $ Env.unsetProjectID


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
        GPUMemory.performGC


insertDependentNode :: Int32 -> Int32 -> Int32 -> Int32 -> RPC Context (SessionST mm) ()
insertDependentNode projectID libraryID nodeID depID = do
    let callPoint = CallPoint (decodeP libraryID) (decodeP nodeID)
    interpreterDo projectID $ Env.insertDependentNode callPoint $ decodeP depID
