---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.ProjectManager.Handler.AST where

import qualified Data.IORef                                                                        as IORef
import qualified Flowbox.Batch.Handler.AST                                                         as BatchAST
import qualified Flowbox.Luna.Data.AST.Crumb.Crumb                                                 as Crumb
import qualified Flowbox.Luna.Data.AST.Expr                                                        as Expr
import qualified Flowbox.Luna.Data.AST.Module                                                      as Module
import qualified Flowbox.Luna.Data.AST.Type                                                        as Type
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Crumb                               ()
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Expr                                ()
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Focus                               ()
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Module                              ()
import           Flowbox.Prelude                                                                   hiding (cons)
import           Flowbox.ProjectManager.Context                                                    (ContextRef)
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Add.Request               as AddData
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Add.Update                as AddData
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Update.Classes.Request    as UpdateDataClasses
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Update.Classes.Update     as UpdateDataClasses
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Update.Cls.Request        as UpdateDataCls
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Update.Cls.Update         as UpdateDataCls
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Update.Cons.Request       as UpdateDataCons
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Update.Cons.Update        as UpdateDataCons
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Update.Methods.Request    as UpdateDataMethods
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Update.Methods.Update     as UpdateDataMethods
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Add.Request           as AddFunction
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Add.Update            as AddFunction
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Update.Inputs.Request as UpdateFunctionInputs
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Update.Inputs.Update  as UpdateFunctionInputs
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Update.Name.Request   as UpdateFunctionName
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Update.Name.Update    as UpdateFunctionName
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Update.Output.Request as UpdateFunctionOutput
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Update.Output.Update  as UpdateFunctionOutput
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Update.Path.Request   as UpdateFunctionPath
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Update.Path.Update    as UpdateFunctionPath
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Get.Request                    as Definitions
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Get.Status                     as Definitions
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Add.Request             as AddModule
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Add.Update              as AddModule
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Update.Cls.Request      as UpdateModuleCls
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Update.Cls.Update       as UpdateModuleCls
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Update.Fields.Request   as UpdateModuleFields
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Update.Fields.Update    as UpdateModuleFields
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Update.Imports.Request  as UpdateModuleImports
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Update.Imports.Update   as UpdateModuleImports
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Remove.Request                 as Remove
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Remove.Update                  as Remove
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Resolve.Request                as ResolveDefinition
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Resolve.Status                 as ResolveDefinition


loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.ProjectManager.Handler.AST"

-------- public api -------------------------------------------------

get :: ContextRef -> Definitions.Request -> IO Definitions.Status
get ctxRef (Definitions.Request mtmaxDepth tbc tlibID tprojectID) = do
    bc  <- decode tbc
    let mmaxDepth = fmap decodeP mtmaxDepth
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    focus <- BatchAST.definitions mmaxDepth bc libID projectID batch
    return $ Definitions.Status (encode focus) tbc tlibID tprojectID


moduleAdd :: ContextRef -> AddModule.Request -> IO AddModule.Update
moduleAdd ctxRef (AddModule.Request tnewModule tbcParent tlibID tprojectID) = do
    newModule <- decode tnewModule
    bcParent  <- decode tbcParent
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    (newBatch, addedModule) <- BatchAST.addModule newModule bcParent libID projectID batch
    IORef.writeIORef ctxRef newBatch
    let newBC = bcParent ++ [Crumb.ModuleCrumb $ addedModule ^. Module.cls . Type.name]
    return $ AddModule.Update (encode addedModule) (encode newBC) tlibID tprojectID


dataAdd :: ContextRef -> AddData.Request -> IO AddData.Update
dataAdd ctxRef (AddData.Request tnewData tbcParent tlibID tprojectID) = do
    newData  <- decode tnewData
    bcParent <- decode tbcParent
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    (newBatch, addedData) <- BatchAST.addClass newData bcParent libID projectID batch
    IORef.writeIORef ctxRef newBatch
    let newBC = bcParent ++ [Crumb.ClassCrumb $ addedData ^. Expr.cls . Type.name]
    return $ AddData.Update (encode addedData) (encode newBC) tlibID tprojectID


functionAdd :: ContextRef -> AddFunction.Request -> IO AddFunction.Update
functionAdd ctxRef (AddFunction.Request tnewFunction tbcParent tlibID tprojectID) = do
    newFunction <- decode tnewFunction
    bcParent    <- decode tbcParent
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    (newBatch, addedFunction) <- BatchAST.addFunction newFunction bcParent libID projectID batch
    IORef.writeIORef ctxRef newBatch
    let newBC = bcParent ++ [Crumb.FunctionCrumb (addedFunction ^. Expr.name) (addedFunction ^. Expr.path)]
    return $ AddFunction.Update (encode addedFunction) (encode newBC) tlibID tprojectID


remove :: ContextRef -> Remove.Request -> IO Remove.Update
remove ctxRef (Remove.Request tbc tlibID tprojectID) = do
    bc  <- decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchAST.remove bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return $ Remove.Update tbc tlibID tprojectID


resolve :: ContextRef -> ResolveDefinition.Request -> IO ResolveDefinition.Status
resolve ctxRef (ResolveDefinition.Request tname tbc tlibID tprojectID) = do
    bc  <- decode tbc
    let name      = decodeP tname
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    results <- BatchAST.resolveDefinition name bc libID projectID batch
    return $ ResolveDefinition.Status (encodeList results) tbc tlibID tprojectID


moduleClsUpdate :: ContextRef -> UpdateModuleCls.Request -> IO UpdateModuleCls.Update
moduleClsUpdate ctxRef (UpdateModuleCls.Request tcls tbc tlibID tprojectID) = do
    cls <- decode tcls
    bc  <- decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchAST.updateModuleCls cls bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return $ UpdateModuleCls.Update tcls tbc tlibID tprojectID


moduleImportsUpdate :: ContextRef -> UpdateModuleImports.Request -> IO UpdateModuleImports.Update
moduleImportsUpdate ctxRef (UpdateModuleImports.Request timports tbc tlibID tprojectID) = do
    imports <- decodeList timports
    bc      <- decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchAST.updateModuleImports imports bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return $ UpdateModuleImports.Update timports tbc tlibID tprojectID


moduleFieldsUpdate :: ContextRef -> UpdateModuleFields.Request -> IO UpdateModuleFields.Update
moduleFieldsUpdate ctxRef (UpdateModuleFields.Request tfields tbc tlibID tprojectID) = do
    fields <- decodeList tfields
    bc     <- decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchAST.updateModuleFields fields bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return $ UpdateModuleFields.Update tfields tbc tlibID tprojectID


dataClsUpdate :: ContextRef -> UpdateDataCls.Request -> IO UpdateDataCls.Update
dataClsUpdate ctxRef (UpdateDataCls.Request tcls tbc tlibID tprojectID) = do
    cls <- decode tcls
    bc  <- decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchAST.updateDataCls cls bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return $ UpdateDataCls.Update tcls tbc tlibID tprojectID


dataConsUpdate :: ContextRef -> UpdateDataCons.Request -> IO UpdateDataCons.Update
dataConsUpdate ctxRef (UpdateDataCons.Request tcons tbc tlibID tprojectID) = do
    cons <- decodeList tcons
    bc   <- decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchAST.updateDataCons cons bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return $ UpdateDataCons.Update tcons tbc tlibID tprojectID


dataClassesUpdate :: ContextRef -> UpdateDataClasses.Request -> IO UpdateDataClasses.Update
dataClassesUpdate ctxRef (UpdateDataClasses.Request tclasses tbc tlibID tprojectID) = do
    classes <- decodeList tclasses
    bc      <- decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchAST.updateDataClasses classes bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return $ UpdateDataClasses.Update tclasses tbc tlibID tprojectID


dataMethodsUpdate :: ContextRef -> UpdateDataMethods.Request -> IO UpdateDataMethods.Update
dataMethodsUpdate ctxRef (UpdateDataMethods.Request tmethods tbc tlibID tprojectID) = do
    methods <- decodeList tmethods
    bc      <- decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchAST.updateDataMethods methods bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return $ UpdateDataMethods.Update tmethods tbc tlibID tprojectID


functionNameUpdate :: ContextRef -> UpdateFunctionName.Request -> IO UpdateFunctionName.Update
functionNameUpdate ctxRef (UpdateFunctionName.Request tname tbc tlibID tprojectID) = do
    bc <- decode tbc
    let name      = decodeP tname
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchAST.updateFunctionName name bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return $ UpdateFunctionName.Update tname tbc tlibID tprojectID


functionPathUpdate :: ContextRef -> UpdateFunctionPath.Request -> IO UpdateFunctionPath.Update
functionPathUpdate ctxRef (UpdateFunctionPath.Request tpath tbc tlibID tprojectID) = do
    bc <- decode tbc
    let path      = decodeListP tpath
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchAST.updateFunctionPath path bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return $ UpdateFunctionPath.Update tpath tbc tlibID tprojectID


functionInputsUpdate :: ContextRef -> UpdateFunctionInputs.Request -> IO UpdateFunctionInputs.Update
functionInputsUpdate ctxRef (UpdateFunctionInputs.Request tinputs tbc tlibID tprojectID) = do
    inputs <- decodeList tinputs
    bc     <- decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchAST.updateFunctionInputs inputs bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return $ UpdateFunctionInputs.Update tinputs tbc tlibID tprojectID


functionOutputUpdate :: ContextRef -> UpdateFunctionOutput.Request -> IO UpdateFunctionOutput.Update
functionOutputUpdate ctxRef (UpdateFunctionOutput.Request toutput tbc tlibID tprojectID) = do
    output <- decode toutput
    bc     <- decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchAST.updateFunctionOutput output bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return $ UpdateFunctionOutput.Update toutput tbc tlibID tprojectID
