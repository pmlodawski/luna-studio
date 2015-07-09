---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Luna.Interpreter.RPC.Handler.ASTWatch where

import qualified Flowbox.Batch.Handler.Common                                                                  as Batch
import qualified Flowbox.Batch.Handler.Graph                                                                   as Batch
import           Flowbox.Bus.RPC.RPC                                                                           (RPC)
import           Flowbox.Control.Error                                                                         hiding (err)
import           Flowbox.Data.Convert
import           Flowbox.Prelude                                                                               hiding (Context, error, op)
import           Flowbox.ProjectManager.Context                                                                (Context)
import qualified Flowbox.ProjectManager.RPC.Handler.AST                                                        as ASTHandler
import qualified Flowbox.ProjectManager.RPC.Handler.Graph                                                      as GraphHandler
import qualified Flowbox.ProjectManager.RPC.Handler.Library                                                    as LibraryHandler
import qualified Flowbox.ProjectManager.RPC.Handler.NodeDefault                                                as NodeDefaultHandler
import qualified Flowbox.ProjectManager.RPC.Handler.Project                                                    as ProjectHandler
import qualified Flowbox.ProjectManager.RPC.Handler.Properties                                                 as PropertiesHandler
import           Flowbox.System.Log.Logger
import qualified Generated.Proto.Dep.Graph.Node                                                                as Gen.Node
import qualified Generated.Proto.Dep.Library.Library                                                           as Gen.Library
import qualified Generated.Proto.Project.Project                                                               as Gen.Project
import qualified Generated.Proto.ProjectManager.Project.Close.Request                                          as ProjectClose
import qualified Generated.Proto.ProjectManager.Project.Close.Update                                           as ProjectClose
import qualified Generated.Proto.ProjectManager.Project.Create.Update                                          as ProjectCreate
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Add.Request                           as ASTDataAdd
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Add.Update                            as ASTDataAdd
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Con_.Add.Request                      as ASTDataConAdd
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Con_.Add.Update                       as ASTDataConAdd
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Con_.Delete.Request                   as ASTDataConDelete
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Con_.Delete.Update                    as ASTDataConDelete
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Con_.Field.Add.Request                as ASTDataConFieldAdd
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Con_.Field.Add.Update                 as ASTDataConFieldAdd
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Con_.Field.Delete.Request             as ASTDataConFieldDelete
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Con_.Field.Delete.Update              as ASTDataConFieldDelete
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Con_.Field.Modify.Request             as ASTDataConFieldModify
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Con_.Field.Modify.Update              as ASTDataConFieldModify
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Con_.Modify.Request                   as ASTDataConModify
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Con_.Modify.Update                    as ASTDataConModify
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
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Graph.Node.Properties.Set.Update  as GraphNodePropertiesSet
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
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Properties.Set.Update                      as ASTPropertiesSet
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Remove.Request                             as ASTRemove
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Remove.Update                              as ASTRemove
import qualified Generated.Proto.ProjectManager.Project.Library.Create.Request                                 as LibraryCreate
import qualified Generated.Proto.ProjectManager.Project.Library.Create.Update                                  as LibraryCreate
import qualified Generated.Proto.ProjectManager.Project.Library.Load.Request                                   as LibraryLoad
import qualified Generated.Proto.ProjectManager.Project.Library.Load.Update                                    as LibraryLoad
import qualified Generated.Proto.ProjectManager.Project.Library.Unload.Request                                 as LibraryUnload
import qualified Generated.Proto.ProjectManager.Project.Library.Unload.Update                                  as LibraryUnload
import qualified Generated.Proto.ProjectManager.Project.Modify.Update                                          as ProjectModify
import qualified Generated.Proto.ProjectManager.Project.Open.Update                                            as ProjectOpen
import qualified Luna.DEP.Graph.Node                                                                           as Node
import           Luna.DEP.Graph.View.Default.Expr                                                              (DefaultExpr (DefaultExpr))
import           Luna.Interpreter.Proto.CallPointPath                                                          ()
import qualified Luna.Interpreter.RPC.Handler.Cache                                                            as CacheWrapper
import           Luna.Interpreter.RPC.Handler.Sync                                                             (sync)
import qualified Luna.Interpreter.RPC.Handler.Var                                                              as Var
import qualified Luna.Interpreter.Session.Memory.GPU                                                           as GPUMemory
import           Luna.Interpreter.Session.Memory.Manager                                                       (MemoryManager)
import           Luna.Interpreter.Session.Session                                                              (SessionST)



logger :: LoggerIO
logger = getLoggerIO $moduleName


--- handlers --------------------------------------------------------------

projectCreate :: MemoryManager mm => ProjectCreate.Update -> RPC Context (SessionST mm) ()
projectCreate (ProjectCreate.Update request project updateNo) = do
    sync updateNo $ ProjectHandler.create request
    let projectID = Gen.Project.id project
    CacheWrapper.modifyAll projectID


projectOpen :: MemoryManager mm => ProjectOpen.Update -> RPC Context (SessionST mm) ()
projectOpen (ProjectOpen.Update _ project _) = do
    let projectID = Gen.Project.id project
    CacheWrapper.modifyAll projectID


projectClose :: ProjectClose.Update -> RPC Context (SessionST mm) ()
projectClose (ProjectClose.Update request updateNo) = do
    sync updateNo $ ProjectHandler.close request Nothing
    CacheWrapper.closeProject $ ProjectClose.projectID request


projectModify :: ProjectModify.Update -> RPC Context (SessionST mm) ()
projectModify (ProjectModify.Update request updateNo) =
    sync updateNo $ ProjectHandler.modify request


libraryCreate :: LibraryCreate.Update -> RPC Context (SessionST mm) ()
libraryCreate (LibraryCreate.Update request library updateNo) = do
    sync updateNo $ LibraryHandler.create request
    let projectID = LibraryCreate.projectID request
    libraryID <- Gen.Library.id library <??> "ASTWatch.libraryCreate : 'libraryID' field is missing"
    CacheWrapper.modifyLibrary projectID libraryID


libraryLoad :: LibraryLoad.Update -> RPC Context (SessionST mm) ()
libraryLoad (LibraryLoad.Update request library _) = do
    let projectID = LibraryLoad.projectID request
    libraryID <- Gen.Library.id library <??> "ASTWatch.libraryLoad : 'libraryID' field is missing"
    CacheWrapper.modifyLibrary projectID libraryID


libraryUnload :: LibraryUnload.Update -> RPC Context (SessionST mm) ()
libraryUnload (LibraryUnload.Update request updateNo) = do
    sync updateNo $ LibraryHandler.unload request
    let projectID = LibraryUnload.projectID request
        libraryID = LibraryUnload.libraryID request
    CacheWrapper.modifyLibrary projectID libraryID


astRemove :: ASTRemove.Update -> RPC Context (SessionST mm) ()
astRemove (ASTRemove.Update request updateNo) = do
    sync updateNo $ ASTHandler.remove request Nothing
    let projectID = ASTRemove.projectID request
        libraryID = ASTRemove.libraryID request
        bc        = ASTRemove.bc request
    CacheWrapper.modifyBreadcrumbsRec projectID libraryID bc


astModuleAdd :: ASTModuleAdd.Update -> RPC Context (SessionST mm) ()
astModuleAdd (ASTModuleAdd.Update request _ bc updateNo) = do
    sync updateNo $ ASTHandler.addModule request Nothing
    let projectID = ASTModuleAdd.projectID request
        libraryID = ASTModuleAdd.libraryID request
    CacheWrapper.modifyBreadcrumbsRec projectID libraryID bc


astModuleModifyCls :: ASTModuleModifyCls.Update -> RPC Context (SessionST mm) ()
astModuleModifyCls (ASTModuleModifyCls.Update request updateNo) = do
    sync updateNo $ ASTHandler.modifyModuleCls request
    let projectID = ASTModuleModifyCls.projectID request
        libraryID = ASTModuleModifyCls.libraryID request
        bc        = ASTModuleModifyCls.bc request
    CacheWrapper.modifyBreadcrumbsRec projectID libraryID bc


astModuleModifyFields :: ASTModuleModifyFields.Update -> RPC Context (SessionST mm) ()
astModuleModifyFields (ASTModuleModifyFields.Update request updateNo) = do
    sync updateNo $ ASTHandler.modifyModuleFields request
    let projectID = ASTModuleModifyFields.projectID request
        libraryID = ASTModuleModifyFields.libraryID request
        bc        = ASTModuleModifyFields.bc request
    CacheWrapper.modifyBreadcrumbsRec projectID libraryID bc


astModuleModifyImports :: ASTModuleModifyImports.Update -> RPC Context (SessionST mm) ()
astModuleModifyImports (ASTModuleModifyImports.Update request updateNo) = do
    sync updateNo $ ASTHandler.modifyModuleImports request
    let projectID = ASTModuleModifyImports.projectID request
        libraryID = ASTModuleModifyImports.libraryID request
        bc        = ASTModuleModifyImports.bc request
    CacheWrapper.modifyBreadcrumbsRec projectID libraryID bc


astDataAdd :: ASTDataAdd.Update -> RPC Context (SessionST mm) ()
astDataAdd (ASTDataAdd.Update request _ bc updateNo) = do
    sync updateNo $ ASTHandler.addData request Nothing
    let projectID = ASTDataAdd.projectID request
        libraryID = ASTDataAdd.libraryID request
    CacheWrapper.modifyBreadcrumbsRec projectID libraryID bc


astDataModifyClasses :: ASTDataModifyClasses.Update -> RPC Context (SessionST mm) ()
astDataModifyClasses (ASTDataModifyClasses.Update request updateNo) = do
    sync updateNo $ ASTHandler.modifyDataClasses request
    let projectID = ASTDataModifyClasses.projectID request
        libraryID = ASTDataModifyClasses.libraryID request
        bc        = ASTDataModifyClasses.bc request
    CacheWrapper.modifyBreadcrumbsRec projectID libraryID bc

astDataModifyCls :: ASTDataModifyCls.Update -> RPC Context (SessionST mm) ()
astDataModifyCls (ASTDataModifyCls.Update request updateNo) = do
    sync updateNo $ ASTHandler.modifyDataCls request Nothing
    let projectID = ASTDataModifyCls.projectID request
        libraryID = ASTDataModifyCls.libraryID request
        bc        = ASTDataModifyCls.bc request
    CacheWrapper.modifyBreadcrumbsRec projectID libraryID bc

astDataConModify :: ASTDataConModify.Update -> RPC Context (SessionST mm) ()
astDataConModify (ASTDataConModify.Update request updateNo) = do
    sync updateNo $ ASTHandler.modifyDataCon request
    let projectID = ASTDataConModify.projectID request
        libraryID = ASTDataConModify.libraryID request
        bc        = ASTDataConModify.bc request
    CacheWrapper.modifyBreadcrumbsRec projectID libraryID bc

astDataConAdd :: ASTDataConAdd.Update -> RPC Context (SessionST mm) ()
astDataConAdd (ASTDataConAdd.Update request _ updateNo) = do
    sync updateNo $ ASTHandler.addDataCon request
    let projectID = ASTDataConAdd.projectID request
        libraryID = ASTDataConAdd.libraryID request
        bc        = ASTDataConAdd.bc request
    CacheWrapper.modifyBreadcrumbsRec projectID libraryID bc

astDataConDelete :: ASTDataConDelete.Update -> RPC Context (SessionST mm) ()
astDataConDelete (ASTDataConDelete.Update request updateNo) = do
    sync updateNo $ ASTHandler.deleteDataCon request
    let projectID = ASTDataConDelete.projectID request
        libraryID = ASTDataConDelete.libraryID request
        bc        = ASTDataConDelete.bc request
    CacheWrapper.modifyBreadcrumbsRec projectID libraryID bc

astDataConFieldAdd :: ASTDataConFieldAdd.Update -> RPC Context (SessionST mm) ()
astDataConFieldAdd (ASTDataConFieldAdd.Update request _ updateNo) = do
    sync updateNo $ ASTHandler.addDataConField request
    let projectID = ASTDataConFieldAdd.projectID request
        libraryID = ASTDataConFieldAdd.libraryID request
        bc        = ASTDataConFieldAdd.bc request
    CacheWrapper.modifyBreadcrumbsRec projectID libraryID bc

astDataConFieldDelete :: ASTDataConFieldDelete.Update -> RPC Context (SessionST mm) ()
astDataConFieldDelete (ASTDataConFieldDelete.Update request updateNo) = do
    sync updateNo $ ASTHandler.deleteDataConField request
    let projectID = ASTDataConFieldDelete.projectID request
        libraryID = ASTDataConFieldDelete.libraryID request
        bc        = ASTDataConFieldDelete.bc request
    CacheWrapper.modifyBreadcrumbsRec projectID libraryID bc

astDataConFieldModify :: ASTDataConFieldModify.Update -> RPC Context (SessionST mm) ()
astDataConFieldModify (ASTDataConFieldModify.Update request updateNo) = do
    sync updateNo $ ASTHandler.modifyDataConField request
    let projectID = ASTDataConFieldModify.projectID request
        libraryID = ASTDataConFieldModify.libraryID request
        bc        = ASTDataConFieldModify.bc request
    CacheWrapper.modifyBreadcrumbsRec projectID libraryID bc


astDataModifyCons :: ASTDataModifyCons.Update -> RPC Context (SessionST mm) ()
astDataModifyCons (ASTDataModifyCons.Update request updateNo) = do
    sync updateNo $ ASTHandler.modifyDataCons request
    let projectID = ASTDataModifyCons.projectID request
        libraryID = ASTDataModifyCons.libraryID request
        bc        = ASTDataModifyCons.bc request
    CacheWrapper.modifyBreadcrumbsRec projectID libraryID bc

astDataModifyMethods :: ASTDataModifyMethods.Update -> RPC Context (SessionST mm) ()
astDataModifyMethods (ASTDataModifyMethods.Update request updateNo) = do
    sync updateNo $ ASTHandler.modifyDataMethods request
    let projectID = ASTDataModifyMethods.projectID request
        libraryID = ASTDataModifyMethods.libraryID request
        bc        = ASTDataModifyMethods.bc request
    CacheWrapper.modifyBreadcrumbsRec projectID libraryID bc

astFunctionAdd :: ASTFunctionAdd.Update -> RPC Context (SessionST mm) ()
astFunctionAdd (ASTFunctionAdd.Update request _ bc updateNo) = do
    sync updateNo $ ASTHandler.addFunction request Nothing
    let projectID = ASTFunctionAdd.projectID request
        libraryID = ASTFunctionAdd.libraryID request
    CacheWrapper.modifyBreadcrumbsRec projectID libraryID bc


astFunctionModifyInputs :: ASTFunctionModifyInputs.Update -> RPC Context (SessionST mm) ()
astFunctionModifyInputs (ASTFunctionModifyInputs.Update request updateNo) = do
    sync updateNo $ ASTHandler.modifyFunctionInputs request Nothing
    let projectID = ASTFunctionModifyInputs.projectID request
        libraryID = ASTFunctionModifyInputs.libraryID request
        bc        = ASTFunctionModifyInputs.bc request
    CacheWrapper.modifyBreadcrumbsRec projectID libraryID bc


astFunctionModifyName :: ASTFunctionModifyName.Update -> RPC Context (SessionST mm) ()
astFunctionModifyName (ASTFunctionModifyName.Update request updateNo) = do
    sync updateNo $ ASTHandler.modifyFunctionName request Nothing
    let projectID = ASTFunctionModifyName.projectID request
        libraryID = ASTFunctionModifyName.libraryID request
        bc        = ASTFunctionModifyName.bc request
    CacheWrapper.modifyBreadcrumbsRec projectID libraryID bc


astFunctionModifyOutput :: ASTFunctionModifyOutput.Update -> RPC Context (SessionST mm) ()
astFunctionModifyOutput (ASTFunctionModifyOutput.Update request updateNo) = do
    sync updateNo $ ASTHandler.modifyFunctionOutput request Nothing
    let projectID = ASTFunctionModifyOutput.projectID request
        libraryID = ASTFunctionModifyOutput.libraryID request
        bc        = ASTFunctionModifyOutput.bc request
    CacheWrapper.modifyBreadcrumbsRec projectID libraryID bc


astFunctionModifyPath :: ASTFunctionModifyPath.Update -> RPC Context (SessionST mm) ()
astFunctionModifyPath (ASTFunctionModifyPath.Update request updateNo) = do
    sync updateNo $ ASTHandler.modifyFunctionPath request
    let projectID = ASTFunctionModifyPath.projectID request
        libraryID = ASTFunctionModifyPath.libraryID request
        bc        = ASTFunctionModifyPath.bc request
    CacheWrapper.modifyBreadcrumbsRec projectID libraryID bc


astPropertiesSet :: ASTPropertiesSet.Update -> RPC Context (SessionST mm) ()
astPropertiesSet (ASTPropertiesSet.Update request updateNo) =
    sync updateNo $ PropertiesHandler.setASTProperties request Nothing


graphConnect :: GraphConnect.Update -> RPC Context (SessionST mm) ()
graphConnect (GraphConnect.Update request updateNo) = do
    sync updateNo $ GraphHandler.connect request Nothing
    let projectID = GraphConnect.projectID request
        libraryID = GraphConnect.libraryID request
        dstID     = GraphConnect.dstNodeID request
    CacheWrapper.modifyNode projectID libraryID dstID


graphDisconnect :: GraphDisconnect.Update -> RPC Context (SessionST mm) ()
graphDisconnect (GraphDisconnect.Update request updateNo) = do
    sync updateNo $ GraphHandler.disconnect request Nothing
    let projectID = GraphDisconnect.projectID request
        libraryID = GraphDisconnect.libraryID request
        dstID     = GraphDisconnect.dstNodeID request
    CacheWrapper.modifyNode projectID libraryID dstID


graphNodeAdd :: GraphNodeAdd.Update -> RPC Context (SessionST mm) ()
graphNodeAdd (GraphNodeAdd.Update request node updateNo) = do
    sync updateNo $ GraphHandler.nodeAdd request Nothing
    let projectID = GraphNodeAdd.projectID request
        libraryID = GraphNodeAdd.libraryID request
    nodeID <- Gen.Node.id node <??> "ASTWatch.graphNodeAdd : 'nodeID' field is missing"
    tNodeExpr <- Gen.Node.expr node <??> "ASTWatch.graphNodeAdd : 'expr' field is missing"
    nodeExpr <- decodeE tNodeExpr
    Var.insertTimeRef' (decodeP libraryID) (decodeP nodeID) nodeExpr --TODO[PM] : remove old refs if were present
    CacheWrapper.modifyNode projectID libraryID nodeID


graphNodeRemove :: MemoryManager mm => GraphNodeRemove.Update -> RPC Context (SessionST mm) ()
graphNodeRemove (GraphNodeRemove.Update request updateNo) = do
    let projectID = GraphNodeRemove.projectID request
        libraryID = GraphNodeRemove.libraryID request
        bc        = GraphNodeRemove.bc request
        nodeIDs   = GraphNodeRemove.nodeIDs request
    mapM_ (CacheWrapper.modifyNodeSuccessors projectID libraryID bc) nodeIDs
    mapM_ (CacheWrapper.deleteNode projectID libraryID) nodeIDs
    CacheWrapper.interpreterDo projectID GPUMemory.performGC
    sync updateNo $ GraphHandler.nodeRemove request Nothing
    --TODO[PM] : remove old time refs if were present


graphNodeModify :: GraphNodeModify.Update -> RPC Context (SessionST mm) ()
graphNodeModify (GraphNodeModify.Update request node updateNo) = do
    sync updateNo $ GraphHandler.nodeModify request Nothing
    let projectID = GraphNodeModify.projectID request
        libraryID = GraphNodeModify.libraryID request
    nodeID     <- Gen.Node.id node <??> "ASTWatch.graphNodeModify : 'nodeID' field is missing"
    case Gen.Node.expr node of
        Nothing       -> return ()
        Just tNodeExpr -> do
            nodeExpr <- decodeE tNodeExpr
            Var.insertTimeRef' (decodeP libraryID) (decodeP nodeID) nodeExpr --TODO[PM] : remove old refs if were present
            CacheWrapper.modifyNode projectID libraryID nodeID


graphNodeModifyInPlace :: GraphNodeModifyInPlace.Update -> RPC Context (SessionST mm) ()
graphNodeModifyInPlace (GraphNodeModifyInPlace.Update request updateNo) = do
    let tprojectID = GraphNodeModifyInPlace.projectID request
        tlibraryID = GraphNodeModifyInPlace.libraryID request
        tnode      = GraphNodeModifyInPlace.node request
        tbc      = GraphNodeModifyInPlace.bc request
    tnodeID   <- Gen.Node.id tnode <??> "ASTWatch.graphNodeModifyInPlace : 'nodeID' field is missing"
    let projectID = decodeP tprojectID
        libraryID = decodeP tlibraryID
        nodeID    = decodeP tnodeID
    bc      <- decodeE tbc
    oldNode <- Batch.nodeByID nodeID bc libraryID projectID
    sync updateNo $ GraphHandler.nodeModifyInPlace request Nothing
    case Gen.Node.expr tnode of
        Nothing -> return ()
        Just tNodeExpr -> do
            nodeExpr <- decodeE tNodeExpr
            Var.insertTimeRef' libraryID nodeID nodeExpr --TODO[PM] : remove old refs if were present
            when (Just nodeExpr /= oldNode ^? Node.expr) $
                CacheWrapper.modifyNode tprojectID tlibraryID tnodeID


graphNodeDefaultRemove :: GraphNodeDefaultRemove.Update -> RPC Context (SessionST mm) ()
graphNodeDefaultRemove (GraphNodeDefaultRemove.Update request updateNo) = do
    sync updateNo $ NodeDefaultHandler.remove request
    let projectID = GraphNodeDefaultRemove.projectID request
        libraryID = GraphNodeDefaultRemove.libraryID request
        nodeID    = GraphNodeDefaultRemove.nodeID request
    CacheWrapper.modifyNode projectID libraryID nodeID
    -- TODO[PM] : remove default from cache


graphNodeDefaultSet :: MemoryManager mm
                    => GraphNodeDefaultSet.Update -> RPC Context (SessionST mm) ()
graphNodeDefaultSet (GraphNodeDefaultSet.Update request updateNo) = do
    let tprojectID = GraphNodeDefaultSet.projectID request
        tlibraryID = GraphNodeDefaultSet.libraryID request
        tnodeID    = GraphNodeDefaultSet.nodeID request
        projectID  = decodeP tprojectID
        libraryID  = decodeP tlibraryID
        nodeID     = decodeP tnodeID
        inPort     = decodeP $ GraphNodeDefaultSet.inPort request
    CacheWrapper.interpreterDo' tprojectID $ do
        Batch.lookupNodeDefault inPort nodeID libraryID projectID >>= \case
            Nothing                          -> return ()
            Just (DefaultExpr defID _ defExpr) -> Var.deleteTimeRef libraryID nodeID defID defExpr
        sync updateNo $ NodeDefaultHandler.set request Nothing
        Batch.lookupNodeDefault inPort nodeID libraryID projectID >>= \case
            Nothing                          -> left "ASTWatch.graphNodeDefaultSet"
            Just (DefaultExpr defID _ defExpr) -> Var.insertTimeRef libraryID nodeID defID defExpr
        -- CacheWrapper.modifyNode tprojectID tlibraryID tnodeID


graphNodePropertiesSet :: GraphNodePropertiesSet.Update -> RPC Context (SessionST mm) ()
graphNodePropertiesSet (GraphNodePropertiesSet.Update request updateNo) =
    sync updateNo $ PropertiesHandler.setNodeProperties request Nothing
