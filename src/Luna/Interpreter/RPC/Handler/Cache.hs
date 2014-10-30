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
import           Luna.Interpreter.Session.Session                       (Session, SessionST)



logger :: LoggerIO
logger = getLoggerIO $(moduleName)

--- helpers ---------------------------------------------------------------

interpreterDo :: Int32 -> Session () -> RPC Context SessionST ()
interpreterDo projectID op = do
    activeProjectID <- liftSession Env.getProjectID
    when (activeProjectID == decodeP projectID) $ liftSession op


deleteAll :: Int32 -> RPC Context SessionST ()
deleteAll projectID = interpreterDo projectID $ do
    Cache.deleteAll
    Cache.performGC


modifyAll :: Int32 -> RPC Context SessionST ()
modifyAll projectID = interpreterDo projectID $ do
    Invalidate.modifyAll
    Cache.performGC


modifyLibrary :: Int32 -> Int32 -> RPC Context SessionST ()
modifyLibrary projectID =
    interpreterDo projectID . Invalidate.modifyLibrary . decodeP


modifyBreadcrumbsRec :: Int32 -> Int32 -> Gen.Breadcrumbs -> RPC Context SessionST ()
modifyBreadcrumbsRec projectID libraryID tbc = do
    bc <- decodeE tbc
    interpreterDo projectID $ Invalidate.modifyBreadcrumbsRec (decodeP libraryID) bc


modifyBreadcrumbs :: Int32 -> Int32 -> Gen.Breadcrumbs -> RPC Context SessionST ()
modifyBreadcrumbs projectID libraryID tbc = do
    bc <- decodeE tbc
    interpreterDo projectID $ Invalidate.modifyBreadcrumbs (decodeP libraryID) bc


modifyNode :: Int32 -> Int32 -> Int32 -> RPC Context SessionST ()
modifyNode projectID libraryID nodeID =
    interpreterDo projectID $ Invalidate.modifyNode (decodeP libraryID) (decodeP nodeID)


modifyNodeSuccessors :: Int32 -> Int32 -> Gen.Breadcrumbs -> Int32 -> RPC Context SessionST ()
modifyNodeSuccessors projectID libraryID tbc nodeID = do
    bc <- decodeE tbc
    interpreterDo projectID $ Invalidate.modifyNodeSuccessors (decodeP libraryID) bc (decodeP nodeID)


deleteNode :: Int32 -> Int32 -> Int32 -> RPC Context SessionST ()
deleteNode projectID libraryID nodeID =
    interpreterDo projectID $ do 
        Cache.deleteNode (decodeP libraryID) (decodeP nodeID)
        Cache.performGC


insertDependentNode :: Int32 -> Int32 -> Int32 -> Int32 -> RPC Context SessionST ()
insertDependentNode projectID libraryID nodeID depID = do
    let callPoint = CallPoint (decodeP libraryID) (decodeP nodeID)
    interpreterDo projectID $ Env.insertDependentNode callPoint $ decodeP depID
