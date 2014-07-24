---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
module Flowbox.Interpreter.RPC.Handler.ASTWatch where

import qualified Text.Read as Read

import qualified Flowbox.Batch.Handler.Common                                                                 as Batch
import qualified Flowbox.Batch.Project.Project                                                                as Project
import           Flowbox.Batch.Project.ProjectManager                                                         (ProjectManager)
import qualified Flowbox.Batch.Project.ProjectManager                                                         as ProjectManager
import           Flowbox.Bus.RPC.RPC                                                                          (RPC)
import           Flowbox.Control.Error                                                                        hiding (err)
import           Flowbox.Control.Monad.Morph
import           Flowbox.Interpreter.Proto.CallPoint                                                          ()
import           Flowbox.Interpreter.Proto.CallPointPath                                                      ()
import qualified Flowbox.Interpreter.Session.Session                                                          as Session
import           Flowbox.Interpreter.Session.SessionT                                                         (SessionT (SessionT))
import           Flowbox.Prelude                                                                              hiding (Context)
import           Flowbox.ProjectManager.Context                                                               (Context)
import qualified Flowbox.ProjectManager.RPC.Handler.AST                                                       as ASTHandler
import qualified Flowbox.ProjectManager.RPC.Handler.Graph                                                     as GraphHandler
import qualified Flowbox.ProjectManager.RPC.Handler.Library                                                   as LibraryHandler
import qualified Flowbox.ProjectManager.RPC.Handler.NodeDefault                                               as NodeDefaultHandler
import qualified Flowbox.ProjectManager.RPC.Handler.Project                                                   as ProjectHandler
import           Flowbox.System.Log.Logger                                                                    hiding (error)
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.ProjectManager.Project.Close.Update                                          as ProjectClose
import qualified Generated.Proto.ProjectManager.Project.Create.Update                                         as ProjectCreate
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Add.Update                           as ASTDataAdd
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Classes.Update                as ASTDataModifyClasses
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Cls.Update                    as ASTDataModifyCls
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Cons.Update                   as ASTDataModifyCons
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Methods.Update                as ASTDataModifyMethods
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Add.Update                       as ASTFunctionAdd
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Connect.Update             as GraphConnect
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Disconnect.Update          as GraphDisconnect
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Add.Update            as GraphNodeAdd
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Default.Remove.Update as GraphNodeDefaultRemove
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Default.Set.Update    as GraphNodeDefaultSet
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Modify.Update         as GraphNodeModify
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.ModifyInPlace.Update  as GraphNodeModifyInPlace
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Remove.Update         as GraphNodeRemove
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Inputs.Update             as ASTFunctionModifyInputs
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Name.Update               as ASTFunctionModifyName
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Output.Update             as ASTFunctionModifyOutput
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Path.Update               as ASTFunctionModifyPath
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Add.Update                         as ASTModuleAdd
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Modify.Cls.Update                  as ASTModuleModifyCls
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Modify.Fields.Update               as ASTModuleModifyFields
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Modify.Imports.Update              as ASTModuleModifyImports
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Remove.Update                             as ASTRemove
import qualified Generated.Proto.ProjectManager.Project.Library.Create.Update                                 as LibraryCreate
import qualified Generated.Proto.ProjectManager.Project.Library.Load.Update                                   as LibraryLoad
import qualified Generated.Proto.ProjectManager.Project.Library.Unload.Update                                 as LibraryUnload
import qualified Generated.Proto.ProjectManager.Project.Open.Update                                           as ProjectOpen
import qualified Generated.Proto.ProjectManager.ProjectManager.Sync.Get.Request                               as ProjectManagerSyncGet
import qualified Generated.Proto.ProjectManager.ProjectManager.Sync.Get.Status                                as ProjectManagerSyncGet



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Interpreter.RPC.Handler.ASTWatch"

--- helpers ---------------------------------------------------------------

syncLibManager :: RPC Context SessionT ()
syncLibManager = do
    pm <- Batch.getProjectManager
    -- TODO [PM] hardcoded project number!
    project <- ProjectManager.lab pm 0 <??> "Project 0 not found"
    lift2 $ SessionT $ Session.setLibManager $ project ^. Project.libs


hoistSessionT :: RPC Context IO a -> RPC Context SessionT ()
hoistSessionT = void . hoist ( hoist $ SessionT . liftIO)

--- handlers --------------------------------------------------------------

projectmanagerSyncGet :: ProjectManagerSyncGet.Status -> RPC Context SessionT ()
projectmanagerSyncGet (ProjectManagerSyncGet.Status _ tdata) = do
    (projectManager :: ProjectManager) <- hoistEither $ Read.readEither $ decodeP tdata
    Batch.setProjectManager projectManager
    syncLibManager


syncIfNeeded :: RPC Context SessionT () -> RPC Context SessionT (Maybe ProjectManagerSyncGet.Request)
syncIfNeeded rpc = do
    result <- lift $ runEitherT rpc
    case result of
        Right () -> return Nothing
        Left err -> do logger error $ "Not syncing : " ++ err
                       return $ Just ProjectManagerSyncGet.Request


projectCreate :: ProjectCreate.Update -> RPC Context SessionT ()
projectCreate (ProjectCreate.Update request _) = do
    hoistSessionT $ ProjectHandler.create request
    syncLibManager


projectOpen :: ProjectOpen.Update -> RPC Context SessionT ()
projectOpen (ProjectOpen.Update request _) = do
    hoistSessionT $ ProjectHandler.open request
    syncLibManager


projectClose :: ProjectClose.Update -> RPC Context SessionT ()
projectClose (ProjectClose.Update request) = do
    hoistSessionT $ ProjectHandler.close request
    syncLibManager


libraryCreate :: LibraryCreate.Update -> RPC Context SessionT ()
libraryCreate (LibraryCreate.Update request _) = do
    hoistSessionT $ LibraryHandler.create request
    syncLibManager


libraryLoad :: LibraryLoad.Update -> RPC Context SessionT ()
libraryLoad (LibraryLoad.Update request _) = do
    hoistSessionT $ LibraryHandler.load request
    syncLibManager


libraryUnload :: LibraryUnload.Update -> RPC Context SessionT ()
libraryUnload (LibraryUnload.Update request) = do
    hoistSessionT $ LibraryHandler.unload request
    syncLibManager


astRemove :: ASTRemove.Update -> RPC Context SessionT ()
astRemove (ASTRemove.Update request) = do
    hoistSessionT $ ASTHandler.remove request
    syncLibManager


astModuleAdd :: ASTModuleAdd.Update -> RPC Context SessionT ()
astModuleAdd (ASTModuleAdd.Update request _ _) = do
    hoistSessionT $ ASTHandler.moduleAdd request
    syncLibManager


astModuleModifyCls :: ASTModuleModifyCls.Update -> RPC Context SessionT ()
astModuleModifyCls (ASTModuleModifyCls.Update request) = do
    hoistSessionT $ ASTHandler.moduleClsModify request
    syncLibManager


astModuleModifyFields :: ASTModuleModifyFields.Update -> RPC Context SessionT ()
astModuleModifyFields (ASTModuleModifyFields.Update request) = do
    hoistSessionT $ ASTHandler.moduleFieldsModify request
    syncLibManager


astModuleModifyImports :: ASTModuleModifyImports.Update -> RPC Context SessionT ()
astModuleModifyImports (ASTModuleModifyImports.Update request) = do
    hoistSessionT $ ASTHandler.moduleImportsModify request
    syncLibManager


astDataAdd :: ASTDataAdd.Update -> RPC Context SessionT ()
astDataAdd (ASTDataAdd.Update request _ _) = do
    hoistSessionT $ ASTHandler.dataAdd request
    syncLibManager


astDataModifyClasses :: ASTDataModifyClasses.Update -> RPC Context SessionT ()
astDataModifyClasses (ASTDataModifyClasses.Update request) = do
    hoistSessionT $ ASTHandler.dataClassesModify request
    syncLibManager


astDataModifyCls :: ASTDataModifyCls.Update -> RPC Context SessionT ()
astDataModifyCls (ASTDataModifyCls.Update request) = do
    hoistSessionT $ ASTHandler.dataClsModify request
    syncLibManager


astDataModifyCons :: ASTDataModifyCons.Update -> RPC Context SessionT ()
astDataModifyCons (ASTDataModifyCons.Update request) = do
    hoistSessionT $ ASTHandler.dataConsModify request
    syncLibManager


astDataModifyMethods :: ASTDataModifyMethods.Update -> RPC Context SessionT ()
astDataModifyMethods (ASTDataModifyMethods.Update request) = do
    hoistSessionT $ ASTHandler.dataMethodsModify request
    syncLibManager


astFunctionAdd :: ASTFunctionAdd.Update -> RPC Context SessionT ()
astFunctionAdd (ASTFunctionAdd.Update request _ _) = do
    hoistSessionT $ ASTHandler.functionAdd request
    syncLibManager


astFunctionModifyInputs :: ASTFunctionModifyInputs.Update -> RPC Context SessionT ()
astFunctionModifyInputs (ASTFunctionModifyInputs.Update request) = do
    hoistSessionT $ ASTHandler.functionInputsModify request
    syncLibManager


astFunctionModifyName :: ASTFunctionModifyName.Update -> RPC Context SessionT ()
astFunctionModifyName (ASTFunctionModifyName.Update request) = do
    hoistSessionT $ ASTHandler.functionNameModify request
    syncLibManager


astFunctionModifyOutput :: ASTFunctionModifyOutput.Update -> RPC Context SessionT ()
astFunctionModifyOutput (ASTFunctionModifyOutput.Update request) = do
    hoistSessionT $ ASTHandler.functionOutputModify request
    syncLibManager


astFunctionModifyPath :: ASTFunctionModifyPath.Update -> RPC Context SessionT ()
astFunctionModifyPath (ASTFunctionModifyPath.Update request) = do
    hoistSessionT $ ASTHandler.functionPathModify request
    syncLibManager


graphConnect :: GraphConnect.Update -> RPC Context SessionT ()
graphConnect (GraphConnect.Update request) = do
    hoistSessionT $ GraphHandler.connect request
    syncLibManager


graphDisconnect :: GraphDisconnect.Update -> RPC Context SessionT ()
graphDisconnect (GraphDisconnect.Update request) = do
    hoistSessionT $ GraphHandler.disconnect request
    syncLibManager


graphNodeAdd :: GraphNodeAdd.Update -> RPC Context SessionT ()
graphNodeAdd (GraphNodeAdd.Update request _) = do
    hoistSessionT $ GraphHandler.nodeAdd request
    syncLibManager


graphNodeRemove :: GraphNodeRemove.Update -> RPC Context SessionT ()
graphNodeRemove (GraphNodeRemove.Update request) = do
    hoistSessionT $ GraphHandler.nodeRemove request
    syncLibManager


graphNodeModify :: GraphNodeModify.Update -> RPC Context SessionT ()
graphNodeModify (GraphNodeModify.Update request _) = do
    hoistSessionT $ GraphHandler.nodeModify request
    syncLibManager


graphNodeModifyInPlace :: GraphNodeModifyInPlace.Update -> RPC Context SessionT ()
graphNodeModifyInPlace (GraphNodeModifyInPlace.Update request) = do
    hoistSessionT $ GraphHandler.nodeModifyInPlace request
    syncLibManager


graphNodeDefaultRemove :: GraphNodeDefaultRemove.Update -> RPC Context SessionT ()
graphNodeDefaultRemove (GraphNodeDefaultRemove.Update request) = do
    hoistSessionT $ NodeDefaultHandler.remove request
    syncLibManager


graphNodeDefaultSet :: GraphNodeDefaultSet.Update -> RPC Context SessionT ()
graphNodeDefaultSet (GraphNodeDefaultSet.Update request) = do
    hoistSessionT $ NodeDefaultHandler.set request
    syncLibManager
