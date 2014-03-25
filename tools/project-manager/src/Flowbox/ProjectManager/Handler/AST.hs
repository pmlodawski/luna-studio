---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.ProjectManager.Handler.AST where

import qualified Data.IORef                                                                       as IORef
import qualified Flowbox.Batch.Handler.AST                                                        as BatchAST
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Crumb                              ()
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Expr                               ()
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Focus                              ()
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Module                             ()
import           Flowbox.Prelude                                                                  hiding (cons)
import           Flowbox.ProjectManager.Context                                                   (ContextRef)
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Add.Args                 as AddClass
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Add.Result               as AddClass
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Update.Classes.Args      as UpdateDataClasses
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Update.Classes.Result    as UpdateDataClasses
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Update.Cls.Args          as UpdateDataCls
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Update.Cls.Result        as UpdateDataCls
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Update.Cons.Args         as UpdateDataCons
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Update.Cons.Result       as UpdateDataCons
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Update.Methods.Args      as UpdateDataMethods
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Update.Methods.Result    as UpdateDataMethods
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Add.Args             as AddFunction
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Add.Result           as AddFunction
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Update.Inputs.Args   as UpdateFunctionInputs
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Update.Inputs.Result as UpdateFunctionInputs
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Update.Name.Args     as UpdateFunctionName
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Update.Name.Result   as UpdateFunctionName
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Update.Output.Args   as UpdateFunctionOutput
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Update.Output.Result as UpdateFunctionOutput
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Update.Path.Args     as UpdateFunctionPath
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Update.Path.Result   as UpdateFunctionPath
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Get.Args                      as Definitions
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Get.Result                    as Definitions
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Add.Args               as AddModule
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Add.Result             as AddModule
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Update.Cls.Args        as UpdateModuleCls
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Update.Cls.Result      as UpdateModuleCls
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Update.Fields.Args     as UpdateModuleFields
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Update.Fields.Result   as UpdateModuleFields
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Update.Imports.Args    as UpdateModuleImports
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Update.Imports.Result  as UpdateModuleImports
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Remove.Args                   as Remove
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Remove.Result                 as Remove
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Resolve.Args                  as ResolveDefinition
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Resolve.Result                as ResolveDefinition



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.ProjectManager.Handler.AST"

-------- public api -------------------------------------------------

get :: ContextRef -> Definitions.Args -> IO Definitions.Result
get ctxRef (Definitions.Args mtmaxDepth tbc tlibID tprojectID) = do
    bc  <- decode tbc
    let mmaxDepth = fmap decodeP mtmaxDepth
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    focus <- BatchAST.definitions mmaxDepth bc libID projectID batch
    return $ Definitions.Result $ encode focus


moduleAdd :: ContextRef -> AddModule.Args -> IO AddModule.Result
moduleAdd ctxRef (AddModule.Args tnewModule tbcParent tlibID tprojectID) = do
    newModule <- decode tnewModule
    bcParent  <- decode tbcParent
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    (newBatch, addedModule) <- BatchAST.addModule newModule bcParent libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return $ AddModule.Result $ encode addedModule


dataAdd :: ContextRef -> AddClass.Args -> IO AddClass.Result
dataAdd ctxRef (AddClass.Args tnewClass tbcParent tlibID tprojectID) = do
    newClass <- decode tnewClass
    bcParent <- decode tbcParent
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    (newBatch, addedClass) <- BatchAST.addClass newClass bcParent libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return $ AddClass.Result $ encode addedClass


functionAdd :: ContextRef -> AddFunction.Args -> IO AddFunction.Result
functionAdd ctxRef (AddFunction.Args tnewFunction tbcParent tlibID tprojectID) = do
    newFunction <- decode tnewFunction
    bcParent    <- decode tbcParent
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    (newBatch, addedFunction) <- BatchAST.addFunction newFunction bcParent libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return $ AddFunction.Result $ encode addedFunction


remove :: ContextRef -> Remove.Args -> IO Remove.Result
remove ctxRef (Remove.Args tbc tlibID tprojectID) = do
    bc  <- decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchAST.remove bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return Remove.Result


resolve :: ContextRef -> ResolveDefinition.Args -> IO ResolveDefinition.Result
resolve ctxRef (ResolveDefinition.Args tname tbc tlibID tprojectID) = do
    bc  <- decode tbc
    let name      = decodeP tname
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    results <- BatchAST.resolveDefinition name bc libID projectID batch
    return $ ResolveDefinition.Result $ encodeList results


moduleClsUpdate :: ContextRef -> UpdateModuleCls.Args -> IO UpdateModuleCls.Result
moduleClsUpdate ctxRef (UpdateModuleCls.Args tcls tbc tlibID tprojectID) = do
    cls <- decode tcls
    bc  <- decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchAST.updateModuleCls cls bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return UpdateModuleCls.Result


moduleImportsUpdate :: ContextRef -> UpdateModuleImports.Args -> IO UpdateModuleImports.Result
moduleImportsUpdate ctxRef (UpdateModuleImports.Args timports tbc tlibID tprojectID) = do
    imports <- decodeList timports
    bc      <- decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchAST.updateModuleImports imports bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return UpdateModuleImports.Result


moduleFieldsUpdate :: ContextRef -> UpdateModuleFields.Args -> IO UpdateModuleFields.Result
moduleFieldsUpdate ctxRef (UpdateModuleFields.Args tfields tbc tlibID tprojectID) = do
    fields <- decodeList tfields
    bc     <- decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchAST.updateModuleFields fields bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return UpdateModuleFields.Result


dataClsUpdate :: ContextRef -> UpdateDataCls.Args -> IO UpdateDataCls.Result
dataClsUpdate ctxRef (UpdateDataCls.Args tcls tbc tlibID tprojectID) = do
    cls <- decode tcls
    bc  <- decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchAST.updateDataCls cls bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return UpdateDataCls.Result


dataConsUpdate :: ContextRef -> UpdateDataCons.Args -> IO UpdateDataCons.Result
dataConsUpdate ctxRef (UpdateDataCons.Args tcons tbc tlibID tprojectID) = do
    cons <- decodeList tcons
    bc   <- decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchAST.updateDataCons cons bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return UpdateDataCons.Result


dataClassesUpdate :: ContextRef -> UpdateDataClasses.Args -> IO UpdateDataClasses.Result
dataClassesUpdate ctxRef (UpdateDataClasses.Args tclasses tbc tlibID tprojectID) = do
    classes <- decodeList tclasses
    bc      <- decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchAST.updateDataClasses classes bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return UpdateDataClasses.Result


dataMethodsUpdate :: ContextRef -> UpdateDataMethods.Args -> IO UpdateDataMethods.Result
dataMethodsUpdate ctxRef (UpdateDataMethods.Args tmethods tbc tlibID tprojectID) = do
    methods <- decodeList tmethods
    bc      <- decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchAST.updateDataMethods methods bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return UpdateDataMethods.Result


functionNameUpdate :: ContextRef -> UpdateFunctionName.Args -> IO UpdateFunctionName.Result
functionNameUpdate ctxRef (UpdateFunctionName.Args tname tbc tlibID tprojectID) = do
    bc <- decode tbc
    let name      = decodeP tname
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchAST.updateFunctionName name bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return UpdateFunctionName.Result


functionPathUpdate :: ContextRef -> UpdateFunctionPath.Args -> IO UpdateFunctionPath.Result
functionPathUpdate ctxRef (UpdateFunctionPath.Args tpath tbc tlibID tprojectID) = do
    bc <- decode tbc
    let path      = decodeListP tpath
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchAST.updateFunctionPath path bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return UpdateFunctionPath.Result


functionInputsUpdate :: ContextRef -> UpdateFunctionInputs.Args -> IO UpdateFunctionInputs.Result
functionInputsUpdate ctxRef (UpdateFunctionInputs.Args tinputs tbc tlibID tprojectID) = do
    inputs <- decodeList tinputs
    bc     <- decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchAST.updateFunctionInputs inputs bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return UpdateFunctionInputs.Result


functionOutputUpdate :: ContextRef -> UpdateFunctionOutput.Args -> IO UpdateFunctionOutput.Result
functionOutputUpdate ctxRef (UpdateFunctionOutput.Args toutput tbc tlibID tprojectID) = do
    output <- decode toutput
    bc     <- decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- IORef.readIORef ctxRef
    newBatch <- BatchAST.updateFunctionOutput output bc libID projectID batch
    IORef.writeIORef ctxRef newBatch
    return UpdateFunctionOutput.Result
