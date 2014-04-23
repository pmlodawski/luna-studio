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
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Classes.Request    as ModifyDataClasses
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Classes.Update     as ModifyDataClasses
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Cls.Request        as ModifyDataCls
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Cls.Update         as ModifyDataCls
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Cons.Request       as ModifyDataCons
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Cons.Update        as ModifyDataCons
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Methods.Request    as ModifyDataMethods
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Methods.Update     as ModifyDataMethods
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Add.Request           as AddFunction
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Add.Update            as AddFunction
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Inputs.Request as ModifyFunctionInputs
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Inputs.Update  as ModifyFunctionInputs
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Name.Request   as ModifyFunctionName
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Name.Update    as ModifyFunctionName
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Output.Request as ModifyFunctionOutput
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Output.Update  as ModifyFunctionOutput
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Path.Request   as ModifyFunctionPath
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Path.Update    as ModifyFunctionPath
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Get.Request                    as Definitions
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Get.Status                     as Definitions
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Add.Request             as AddModule
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Add.Update              as AddModule
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Modify.Cls.Request      as ModifyModuleCls
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Modify.Cls.Update       as ModifyModuleCls
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Modify.Fields.Request   as ModifyModuleFields
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Modify.Fields.Update    as ModifyModuleFields
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Modify.Imports.Request  as ModifyModuleImports
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Modify.Imports.Update   as ModifyModuleImports
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
    return $ Definitions.Status (encode focus) mtmaxDepth tbc tlibID tprojectID


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


moduleClsModify :: ContextRef -> ModifyModuleCls.Request -> IO ModifyModuleCls.Update
moduleClsModify ctxRef (ModifyModuleCls.Request tcls tbc tlibID tprojectID) = do
    cls <- decode tcls
    bc  <- decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchAST.updateModuleCls cls bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return $ ModifyModuleCls.Update tcls tbc tlibID tprojectID


moduleImportsModify :: ContextRef -> ModifyModuleImports.Request -> IO ModifyModuleImports.Update
moduleImportsModify ctxRef (ModifyModuleImports.Request timports tbc tlibID tprojectID) = do
    imports <- decodeList timports
    bc      <- decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchAST.updateModuleImports imports bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return $ ModifyModuleImports.Update timports tbc tlibID tprojectID


moduleFieldsModify :: ContextRef -> ModifyModuleFields.Request -> IO ModifyModuleFields.Update
moduleFieldsModify ctxRef (ModifyModuleFields.Request tfields tbc tlibID tprojectID) = do
    fields <- decodeList tfields
    bc     <- decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchAST.updateModuleFields fields bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return $ ModifyModuleFields.Update tfields tbc tlibID tprojectID


dataClsModify :: ContextRef -> ModifyDataCls.Request -> IO ModifyDataCls.Update
dataClsModify ctxRef (ModifyDataCls.Request tcls tbc tlibID tprojectID) = do
    cls <- decode tcls
    bc  <- decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchAST.updateDataCls cls bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return $ ModifyDataCls.Update tcls tbc tlibID tprojectID


dataConsModify :: ContextRef -> ModifyDataCons.Request -> IO ModifyDataCons.Update
dataConsModify ctxRef (ModifyDataCons.Request tcons tbc tlibID tprojectID) = do
    cons <- decodeList tcons
    bc   <- decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchAST.updateDataCons cons bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return $ ModifyDataCons.Update tcons tbc tlibID tprojectID


dataClassesModify :: ContextRef -> ModifyDataClasses.Request -> IO ModifyDataClasses.Update
dataClassesModify ctxRef (ModifyDataClasses.Request tclasses tbc tlibID tprojectID) = do
    classes <- decodeList tclasses
    bc      <- decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchAST.updateDataClasses classes bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return $ ModifyDataClasses.Update tclasses tbc tlibID tprojectID


dataMethodsModify :: ContextRef -> ModifyDataMethods.Request -> IO ModifyDataMethods.Update
dataMethodsModify ctxRef (ModifyDataMethods.Request tmethods tbc tlibID tprojectID) = do
    methods <- decodeList tmethods
    bc      <- decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchAST.updateDataMethods methods bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return $ ModifyDataMethods.Update tmethods tbc tlibID tprojectID


functionNameModify :: ContextRef -> ModifyFunctionName.Request -> IO ModifyFunctionName.Update
functionNameModify ctxRef (ModifyFunctionName.Request tname tbc tlibID tprojectID) = do
    bc <- decode tbc
    let name      = decodeP tname
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchAST.updateFunctionName name bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return $ ModifyFunctionName.Update tname tbc tlibID tprojectID


functionPathModify :: ContextRef -> ModifyFunctionPath.Request -> IO ModifyFunctionPath.Update
functionPathModify ctxRef (ModifyFunctionPath.Request tpath tbc tlibID tprojectID) = do
    bc <- decode tbc
    let path      = decodeListP tpath
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchAST.updateFunctionPath path bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return $ ModifyFunctionPath.Update tpath tbc tlibID tprojectID


functionInputsModify :: ContextRef -> ModifyFunctionInputs.Request -> IO ModifyFunctionInputs.Update
functionInputsModify ctxRef (ModifyFunctionInputs.Request tinputs tbc tlibID tprojectID) = do
    inputs <- decodeList tinputs
    bc     <- decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchAST.updateFunctionInputs inputs bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return $ ModifyFunctionInputs.Update tinputs tbc tlibID tprojectID


functionOutputModify :: ContextRef -> ModifyFunctionOutput.Request -> IO ModifyFunctionOutput.Update
functionOutputModify ctxRef (ModifyFunctionOutput.Request toutput tbc tlibID tprojectID) = do
    output <- decode toutput
    bc     <- decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchAST.updateFunctionOutput output bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return $ ModifyFunctionOutput.Update toutput tbc tlibID tprojectID
