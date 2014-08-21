---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Luna.Interpreter.RPC.Handler.ASTWatch where

import           Data.Int  (Int32)
import qualified Text.Read as Read

import qualified Flowbox.Batch.Handler.Common                                                                  as Batch
import qualified Flowbox.Batch.Project.Project                                                                 as Project
import           Flowbox.Batch.Project.ProjectManager                                                          (ProjectManager)
import qualified Flowbox.Batch.Project.ProjectManager                                                          as ProjectManager
import           Flowbox.Batch.Tools.Serialize.Proto.Conversion.Project                                        ()
import           Flowbox.Bus.RPC.RPC                                                                           (RPC)
import           Flowbox.Control.Error                                                                         hiding (err)
import           Flowbox.Control.Monad.Morph
import           Flowbox.Prelude                                                                               hiding (Context, op)
import           Flowbox.ProjectManager.Context                                                                (Context)
import qualified Flowbox.ProjectManager.RPC.Handler.AST                                                        as ASTHandler
import qualified Flowbox.ProjectManager.RPC.Handler.Graph                                                      as GraphHandler
import qualified Flowbox.ProjectManager.RPC.Handler.Library                                                    as LibraryHandler
import qualified Flowbox.ProjectManager.RPC.Handler.NodeDefault                                                as NodeDefaultHandler
import qualified Flowbox.ProjectManager.RPC.Handler.Project                                                    as ProjectHandler
import           Flowbox.System.Log.Logger                                                                     hiding (error)
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Crumb.Breadcrumbs                                                             as Gen
import qualified Generated.Proto.Graph.Node                                                                    as Gen.Node
import qualified Generated.Proto.Library.Library                                                               as Gen.Library
import qualified Generated.Proto.Project.Project                                                               as Gen.Project
import qualified Generated.Proto.ProjectManager.Project.Close.Request                                          as ProjectClose
import qualified Generated.Proto.ProjectManager.Project.Close.Update                                           as ProjectClose
import qualified Generated.Proto.ProjectManager.Project.Create.Update                                          as ProjectCreate
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Add.Request                           as ASTDataAdd
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Add.Update                            as ASTDataAdd
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Classes.Request                as ASTDataModifyClasses
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Classes.Update                 as ASTDataModifyClasses
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Cls.Request                    as ASTDataModifyCls
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Cls.Update                     as ASTDataModifyCls
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Cons.Request                   as ASTDataModifyCons
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Cons.Update                    as ASTDataModifyCons
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Methods.Request                as ASTDataModifyMethods
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Methods.Update                 as ASTDataModifyMethods
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Add.Request                       as ASTFunctionAdd
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Add.Update                        as ASTFunctionAdd
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Connect.Request             as GraphConnect
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Connect.Update              as GraphConnect
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Disconnect.Request          as GraphDisconnect
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Disconnect.Update           as GraphDisconnect
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Add.Request            as GraphNodeAdd
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Add.Update             as GraphNodeAdd
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Default.Remove.Request as GraphNodeDefaultRemove
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Default.Remove.Update  as GraphNodeDefaultRemove
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Default.Set.Request    as GraphNodeDefaultSet
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Default.Set.Update     as GraphNodeDefaultSet
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Modify.Request         as GraphNodeModify
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Modify.Update          as GraphNodeModify
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.ModifyInPlace.Request  as GraphNodeModifyInPlace
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.ModifyInPlace.Update   as GraphNodeModifyInPlace
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Remove.Request         as GraphNodeRemove
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Remove.Update          as GraphNodeRemove
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Inputs.Request             as ASTFunctionModifyInputs
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Inputs.Update              as ASTFunctionModifyInputs
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Name.Request               as ASTFunctionModifyName
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Name.Update                as ASTFunctionModifyName
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Output.Request             as ASTFunctionModifyOutput
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Output.Update              as ASTFunctionModifyOutput
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Path.Request               as ASTFunctionModifyPath
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Path.Update                as ASTFunctionModifyPath
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Add.Request                         as ASTModuleAdd
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Add.Update                          as ASTModuleAdd
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Modify.Cls.Request                  as ASTModuleModifyCls
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Modify.Cls.Update                   as ASTModuleModifyCls
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Modify.Fields.Request               as ASTModuleModifyFields
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Modify.Fields.Update                as ASTModuleModifyFields
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Modify.Imports.Request              as ASTModuleModifyImports
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Modify.Imports.Update               as ASTModuleModifyImports
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Remove.Request                             as ASTRemove
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Remove.Update                              as ASTRemove
import qualified Generated.Proto.ProjectManager.Project.Library.Create.Request                                 as LibraryCreate
import qualified Generated.Proto.ProjectManager.Project.Library.Create.Update                                  as LibraryCreate
import qualified Generated.Proto.ProjectManager.Project.Library.Load.Request                                   as LibraryLoad
import qualified Generated.Proto.ProjectManager.Project.Library.Load.Update                                    as LibraryLoad
import qualified Generated.Proto.ProjectManager.Project.Library.Unload.Request                                 as LibraryUnload
import qualified Generated.Proto.ProjectManager.Project.Library.Unload.Update                                  as LibraryUnload
import qualified Generated.Proto.ProjectManager.Project.Open.Update                                            as ProjectOpen
import qualified Generated.Proto.ProjectManager.ProjectManager.Sync.Get.Request                                as ProjectManagerSyncGet
import qualified Generated.Proto.ProjectManager.ProjectManager.Sync.Get.Status                                 as ProjectManagerSyncGet
import           Luna.Interpreter.Proto.CallPoint                                                              ()
import           Luna.Interpreter.Proto.CallPointPath                                                          ()
import qualified Luna.Interpreter.Session.Cache.Invalidate                                                     as Invalidate
import           Luna.Interpreter.Session.Session                                                              (Session)
import qualified Luna.Interpreter.Session.Session                                                              as Session
import           Luna.Interpreter.Session.SessionT                                                             (SessionT (SessionT))



logger :: LoggerIO
logger = getLoggerIO "Luna.Interpreter.RPC.Handler.ASTWatch"

--- helpers ---------------------------------------------------------------

syncLibManager :: Int32 -> RPC Context SessionT ()
syncLibManager updateNo = do
    testUpdateNo updateNo
    pm <- Batch.getProjectManager
    -- TODO [PM] hardcoded project number!
    project <- ProjectManager.lab pm 0 <??> "Project 0 not found"
    lift2 $ SessionT $ Session.setLibManager $ project ^. Project.libs


testUpdateNo :: Int32 -> RPC Context SessionT ()
testUpdateNo updateNo = do
    localUpdateNo <- Batch.getUpdateNo
    assertE (updateNo == localUpdateNo) "UpdateNo does not match"


hoistSessionT :: RPC Context IO a -> RPC Context SessionT ()
hoistSessionT = void . hoist ( hoist $ SessionT . liftIO)


sync :: Int32 -> RPC Context IO a -> RPC Context SessionT ()
sync updateNo syncOp = do
    hoistSessionT $ void syncOp
    syncLibManager updateNo


interpreterDo :: Int32 -> Session () -> RPC Context SessionT ()
interpreterDo projectID op = do
    activeProjectID <- lift2 $ SessionT $ Session.getProjectID
    when (activeProjectID == decodeP projectID) $ lift2 $ SessionT op


modifyAll :: Int32 -> RPC Context SessionT ()
modifyAll projectID = interpreterDo projectID Invalidate.modifyAll


modifyLibrary :: Int32 -> Int32 -> RPC Context SessionT ()
modifyLibrary projectID =
    interpreterDo projectID . Invalidate.modifyLibrary . decodeP


modifyBreadcrumbsRec :: Int32 -> Int32 -> Gen.Breadcrumbs -> RPC Context SessionT ()
modifyBreadcrumbsRec projectID libraryID tbc = do
    bc <- decodeE tbc
    interpreterDo projectID $ Invalidate.modifyBreadcrumbsRec (decodeP libraryID) bc


modifyBreadcrumbs :: Int32 -> Int32 -> Gen.Breadcrumbs -> RPC Context SessionT ()
modifyBreadcrumbs projectID libraryID tbc = do
    bc <- decodeE tbc
    interpreterDo projectID $ Invalidate.modifyBreadcrumbs (decodeP libraryID) bc


modifyNode :: Int32 -> Int32 -> Int32 -> RPC Context SessionT ()
modifyNode projectID libraryID nodeID = do
    interpreterDo projectID $ Invalidate.modifyNode (decodeP libraryID) (decodeP nodeID)


--- handlers --------------------------------------------------------------

projectmanagerSyncGet :: ProjectManagerSyncGet.Status -> RPC Context SessionT ()
projectmanagerSyncGet (ProjectManagerSyncGet.Status _ tdata updateNo) = do
    (projectManager :: ProjectManager) <- hoistEither $ Read.readEither $ decodeP tdata
    Batch.setProjectManager projectManager
    Batch.setUpdateNo updateNo


syncIfNeeded :: RPC Context SessionT () -> RPC Context SessionT (Maybe ProjectManagerSyncGet.Request)
syncIfNeeded rpc = do
    result <- lift $ runEitherT rpc
    case result of
        Right () -> return Nothing
        Left err -> do logger error $ "Not syncing : " ++ err
                       return $ Just ProjectManagerSyncGet.Request


projectCreate :: ProjectCreate.Update -> RPC Context SessionT ()
projectCreate (ProjectCreate.Update request project updateNo) = do
    sync updateNo $ ProjectHandler.create request
    projectID <- Gen.Project.id project <??> "ASTWatch.projectCreate : 'projectID' field is missing"
    modifyAll projectID


projectOpen :: ProjectOpen.Update -> RPC Context SessionT ()
projectOpen (ProjectOpen.Update request project updateNo) = do
    sync updateNo $ ProjectHandler.open request
    projectID <- Gen.Project.id project <??> "ASTWatch.projectOpen : 'projectID' field is missing"
    modifyAll projectID


projectClose :: ProjectClose.Update -> RPC Context SessionT ()
projectClose (ProjectClose.Update request updateNo) = do
    sync updateNo $ ProjectHandler.close request
    modifyAll $ ProjectClose.projectID request


libraryCreate :: LibraryCreate.Update -> RPC Context SessionT ()
libraryCreate (LibraryCreate.Update request library updateNo) = do
    sync updateNo $ LibraryHandler.create request
    let projectID = LibraryCreate.projectID request
    libraryID <- Gen.Library.id library <??> "ASTWatch.libraryCreate : 'libraryID' field is missing"
    modifyLibrary projectID libraryID


libraryLoad :: LibraryLoad.Update -> RPC Context SessionT ()
libraryLoad (LibraryLoad.Update request library updateNo) = do
    sync updateNo $ LibraryHandler.load request
    let projectID = LibraryLoad.projectID request
    libraryID <- Gen.Library.id library <??> "ASTWatch.libraryLoad : 'libraryID' field is missing"
    modifyLibrary projectID libraryID


libraryUnload :: LibraryUnload.Update -> RPC Context SessionT ()
libraryUnload (LibraryUnload.Update request updateNo) = do
    sync updateNo $ LibraryHandler.unload request
    let projectID = LibraryUnload.projectID request
        libraryID = LibraryUnload.libraryID request
    modifyLibrary projectID libraryID


astRemove :: ASTRemove.Update -> RPC Context SessionT ()
astRemove (ASTRemove.Update request updateNo) = do
    sync updateNo $ ASTHandler.remove request
    let projectID = ASTRemove.projectID request
        libraryID = ASTRemove.libraryID request
        bc        = ASTRemove.bc request
    modifyBreadcrumbsRec projectID libraryID bc


astModuleAdd :: ASTModuleAdd.Update -> RPC Context SessionT ()
astModuleAdd (ASTModuleAdd.Update request _ bc updateNo) = do
    sync updateNo $ ASTHandler.moduleAdd request
    let projectID = ASTModuleAdd.projectID request
        libraryID = ASTModuleAdd.libraryID request
    modifyBreadcrumbsRec projectID libraryID bc


astModuleModifyCls :: ASTModuleModifyCls.Update -> RPC Context SessionT ()
astModuleModifyCls (ASTModuleModifyCls.Update request updateNo) = do
    sync updateNo $ ASTHandler.moduleClsModify request
    let projectID = ASTModuleModifyCls.projectID request
        libraryID = ASTModuleModifyCls.libraryID request
        bc        = ASTModuleModifyCls.bc request
    modifyBreadcrumbsRec projectID libraryID bc


astModuleModifyFields :: ASTModuleModifyFields.Update -> RPC Context SessionT ()
astModuleModifyFields (ASTModuleModifyFields.Update request updateNo) = do
    sync updateNo $ ASTHandler.moduleFieldsModify request
    let projectID = ASTModuleModifyFields.projectID request
        libraryID = ASTModuleModifyFields.libraryID request
        bc        = ASTModuleModifyFields.bc request
    modifyBreadcrumbsRec projectID libraryID bc


astModuleModifyImports :: ASTModuleModifyImports.Update -> RPC Context SessionT ()
astModuleModifyImports (ASTModuleModifyImports.Update request updateNo) = do
    sync updateNo $ ASTHandler.moduleImportsModify request
    let projectID = ASTModuleModifyImports.projectID request
        libraryID = ASTModuleModifyImports.libraryID request
        bc        = ASTModuleModifyImports.bc request
    modifyBreadcrumbsRec projectID libraryID bc


astDataAdd :: ASTDataAdd.Update -> RPC Context SessionT ()
astDataAdd (ASTDataAdd.Update request _ bc updateNo) = do
    sync updateNo $ ASTHandler.dataAdd request
    let projectID = ASTDataAdd.projectID request
        libraryID = ASTDataAdd.libraryID request
    modifyBreadcrumbsRec projectID libraryID bc


astDataModifyClasses :: ASTDataModifyClasses.Update -> RPC Context SessionT ()
astDataModifyClasses (ASTDataModifyClasses.Update request updateNo) = do
    sync updateNo $ ASTHandler.dataClassesModify request
    let projectID = ASTDataModifyClasses.projectID request
        libraryID = ASTDataModifyClasses.libraryID request
        bc        = ASTDataModifyClasses.bc request
    modifyBreadcrumbsRec projectID libraryID bc

astDataModifyCls :: ASTDataModifyCls.Update -> RPC Context SessionT ()
astDataModifyCls (ASTDataModifyCls.Update request updateNo) = do
    sync updateNo $ ASTHandler.dataClsModify request
    let projectID = ASTDataModifyCls.projectID request
        libraryID = ASTDataModifyCls.libraryID request
        bc        = ASTDataModifyCls.bc request
    modifyBreadcrumbsRec projectID libraryID bc


astDataModifyCons :: ASTDataModifyCons.Update -> RPC Context SessionT ()
astDataModifyCons (ASTDataModifyCons.Update request updateNo) = do
    sync updateNo $ ASTHandler.dataConsModify request
    let projectID = ASTDataModifyCons.projectID request
        libraryID = ASTDataModifyCons.libraryID request
        bc        = ASTDataModifyCons.bc request
    modifyBreadcrumbsRec projectID libraryID bc

astDataModifyMethods :: ASTDataModifyMethods.Update -> RPC Context SessionT ()
astDataModifyMethods (ASTDataModifyMethods.Update request updateNo) = do
    sync updateNo $ ASTHandler.dataMethodsModify request
    let projectID = ASTDataModifyMethods.projectID request
        libraryID = ASTDataModifyMethods.libraryID request
        bc        = ASTDataModifyMethods.bc request
    modifyBreadcrumbsRec projectID libraryID bc

astFunctionAdd :: ASTFunctionAdd.Update -> RPC Context SessionT ()
astFunctionAdd (ASTFunctionAdd.Update request _ bc updateNo) = do
    sync updateNo $ ASTHandler.functionAdd request
    let projectID = ASTFunctionAdd.projectID request
        libraryID = ASTFunctionAdd.libraryID request
    modifyBreadcrumbsRec projectID libraryID bc


astFunctionModifyInputs :: ASTFunctionModifyInputs.Update -> RPC Context SessionT ()
astFunctionModifyInputs (ASTFunctionModifyInputs.Update request updateNo) = do
    sync updateNo $ ASTHandler.functionInputsModify request
    let projectID = ASTFunctionModifyInputs.projectID request
        libraryID = ASTFunctionModifyInputs.libraryID request
        bc        = ASTFunctionModifyInputs.bc request
    modifyBreadcrumbsRec projectID libraryID bc


astFunctionModifyName :: ASTFunctionModifyName.Update -> RPC Context SessionT ()
astFunctionModifyName (ASTFunctionModifyName.Update request updateNo) = do
    sync updateNo $ ASTHandler.functionNameModify request
    let projectID = ASTFunctionModifyName.projectID request
        libraryID = ASTFunctionModifyName.libraryID request
        bc        = ASTFunctionModifyName.bc request
    modifyBreadcrumbsRec projectID libraryID bc


astFunctionModifyOutput :: ASTFunctionModifyOutput.Update -> RPC Context SessionT ()
astFunctionModifyOutput (ASTFunctionModifyOutput.Update request updateNo) = do
    sync updateNo $ ASTHandler.functionOutputModify request
    let projectID = ASTFunctionModifyOutput.projectID request
        libraryID = ASTFunctionModifyOutput.libraryID request
        bc        = ASTFunctionModifyOutput.bc request
    modifyBreadcrumbsRec projectID libraryID bc


astFunctionModifyPath :: ASTFunctionModifyPath.Update -> RPC Context SessionT ()
astFunctionModifyPath (ASTFunctionModifyPath.Update request updateNo) = do
    sync updateNo $ ASTHandler.functionPathModify request
    let projectID = ASTFunctionModifyPath.projectID request
        libraryID = ASTFunctionModifyPath.libraryID request
        bc        = ASTFunctionModifyPath.bc request
    modifyBreadcrumbsRec projectID libraryID bc


graphConnect :: GraphConnect.Update -> RPC Context SessionT ()
graphConnect (GraphConnect.Update request updateNo) = do
    sync updateNo $ GraphHandler.connect request
    let projectID = GraphConnect.projectID request
        libraryID = GraphConnect.libraryID request
        dstID     = GraphConnect.libraryID request
    modifyNode projectID libraryID dstID


graphDisconnect :: GraphDisconnect.Update -> RPC Context SessionT ()
graphDisconnect (GraphDisconnect.Update request updateNo) = do
    sync updateNo $ GraphHandler.disconnect request
    let projectID = GraphDisconnect.projectID request
        libraryID = GraphDisconnect.libraryID request
        dstID     = GraphDisconnect.libraryID request
    modifyNode projectID libraryID dstID


graphNodeAdd :: GraphNodeAdd.Update -> RPC Context SessionT ()
graphNodeAdd (GraphNodeAdd.Update request node updateNo) = do
    sync updateNo $ GraphHandler.nodeAdd request
    let projectID = GraphNodeAdd.projectID request
        libraryID = GraphNodeAdd.libraryID request
    nodeID <- Gen.Node.id node <??> "ASTWatch.graphNodeAdd : 'nodeID' field is missing"
    modifyNode projectID libraryID nodeID


graphNodeRemove :: GraphNodeRemove.Update -> RPC Context SessionT ()
graphNodeRemove (GraphNodeRemove.Update request updateNo) = do
    sync updateNo $ GraphHandler.nodeRemove request
    let projectID = GraphNodeRemove.projectID request
        libraryID = GraphNodeRemove.libraryID request
        nodeID    = GraphNodeRemove.nodeID request
    modifyNode projectID libraryID nodeID -- FIXME [PM] : invalidate successor nodes


graphNodeModify :: GraphNodeModify.Update -> RPC Context SessionT ()
graphNodeModify (GraphNodeModify.Update request node updateNo) = do
    sync updateNo $ GraphHandler.nodeModify request
    let projectID = GraphNodeModify.projectID request
        libraryID = GraphNodeModify.libraryID request
    nodeID <- Gen.Node.id node <??> "ASTWatch.graphNodeModify : 'nodeID' field is missing"
    modifyNode projectID libraryID nodeID


graphNodeModifyInPlace :: GraphNodeModifyInPlace.Update -> RPC Context SessionT ()
graphNodeModifyInPlace (GraphNodeModifyInPlace.Update request updateNo) = do
    sync updateNo $ GraphHandler.nodeModifyInPlace request
    let projectID = GraphNodeModifyInPlace.projectID request
        libraryID = GraphNodeModifyInPlace.libraryID request
    nodeID <- Gen.Node.id (GraphNodeModifyInPlace.node request) <??> "ASTWatch.graphNodeModify : 'nodeID' field is missing"
    modifyNode projectID libraryID nodeID


graphNodeDefaultRemove :: GraphNodeDefaultRemove.Update -> RPC Context SessionT ()
graphNodeDefaultRemove (GraphNodeDefaultRemove.Update request updateNo) = do
    sync updateNo $ NodeDefaultHandler.remove request
    let projectID = GraphNodeDefaultRemove.projectID request
        libraryID = GraphNodeDefaultRemove.libraryID request
        nodeID    = GraphNodeDefaultRemove.nodeID request
    modifyNode projectID libraryID nodeID


graphNodeDefaultSet :: GraphNodeDefaultSet.Update -> RPC Context SessionT ()
graphNodeDefaultSet (GraphNodeDefaultSet.Update request updateNo) = do
    sync updateNo $ NodeDefaultHandler.set request
    let projectID = GraphNodeDefaultSet.projectID request
        libraryID = GraphNodeDefaultSet.libraryID request
        nodeID    = GraphNodeDefaultSet.nodeID request
    modifyNode projectID libraryID nodeID
