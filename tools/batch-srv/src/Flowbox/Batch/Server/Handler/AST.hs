---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Batch.Server.Handler.AST where

import Data.IORef (IORef)

import           Flowbox.Batch.Batch                                   (Batch)
import qualified Flowbox.Batch.Handler.AST                             as BatchAST
import           Flowbox.Control.Error
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Crumb   ()
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Expr    ()
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Module  ()
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Batch.AST.AddClass.Args               as AddClass
import qualified Generated.Proto.Batch.AST.AddClass.Result             as AddClass
import qualified Generated.Proto.Batch.AST.AddFunction.Args            as AddFunction
import qualified Generated.Proto.Batch.AST.AddFunction.Result          as AddFunction
import qualified Generated.Proto.Batch.AST.AddModule.Args              as AddModule
import qualified Generated.Proto.Batch.AST.AddModule.Result            as AddModule
import qualified Generated.Proto.Batch.AST.Remove.Args                 as Remove
import qualified Generated.Proto.Batch.AST.Remove.Result               as Remove
import qualified Generated.Proto.Batch.AST.UpdateClassCls.Args         as UpdateClassCls
import qualified Generated.Proto.Batch.AST.UpdateClassCls.Result       as UpdateClassCls
import qualified Generated.Proto.Batch.AST.UpdateClassFields.Args      as UpdateClassFields
import qualified Generated.Proto.Batch.AST.UpdateClassFields.Result    as UpdateClassFields
import qualified Generated.Proto.Batch.AST.UpdateFunctionInputs.Args   as UpdateFunctionInputs
import qualified Generated.Proto.Batch.AST.UpdateFunctionInputs.Result as UpdateFunctionInputs
import qualified Generated.Proto.Batch.AST.UpdateFunctionName.Args     as UpdateFunctionName
import qualified Generated.Proto.Batch.AST.UpdateFunctionName.Result   as UpdateFunctionName
import qualified Generated.Proto.Batch.AST.UpdateFunctionOutput.Args   as UpdateFunctionOutput
import qualified Generated.Proto.Batch.AST.UpdateFunctionOutput.Result as UpdateFunctionOutput
import qualified Generated.Proto.Batch.AST.UpdateFunctionPath.Args     as UpdateFunctionPath
import qualified Generated.Proto.Batch.AST.UpdateFunctionPath.Result   as UpdateFunctionPath
import qualified Generated.Proto.Batch.AST.UpdateModuleCls.Args        as UpdateModuleCls
import qualified Generated.Proto.Batch.AST.UpdateModuleCls.Result      as UpdateModuleCls
import qualified Generated.Proto.Batch.AST.UpdateModuleFields.Args     as UpdateModuleFields
import qualified Generated.Proto.Batch.AST.UpdateModuleFields.Result   as UpdateModuleFields
import qualified Generated.Proto.Batch.AST.UpdateModuleImports.Args    as UpdateModuleImports
import qualified Generated.Proto.Batch.AST.UpdateModuleImports.Result  as UpdateModuleImports


loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.Handlers.AST"

-------- public api -------------------------------------------------

--definitions :: IORef Batch -> Definitions.Args -> Script Definitions.Result
--definitions maxDepth bc libID projectID = readonly . astOp libID projectID (\_ ast -> do
--    loggerIO warning "maxDepth and breadcrumbs are not yet implemented. Returning whole AST from root."
--    shrinked <- Shrink.shrinkFunctionBodies ast
--    return (ast, shrinked))


addModule :: IORef Batch -> AddModule.Args -> Script AddModule.Result
addModule batchHandler (AddModule.Args tnewModule tbcParent tlibID tprojectID) = do
    scriptIO $ loggerIO info "called addModule"
    newModule <- tryRight $ decode tnewModule
    bcParent  <- tryRight $ decode tbcParent
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- tryReadIORef batchHandler
    newBatch <- scriptIO $ BatchAST.addModule newModule bcParent libID projectID batch
    tryWriteIORef batchHandler newBatch
    return AddModule.Result


addClass :: IORef Batch -> AddClass.Args -> Script AddClass.Result
addClass batchHandler (AddClass.Args tnewClass tbcParent tlibID tprojectID) = do
    scriptIO $ loggerIO info "called addClass"
    newClass <- tryRight $ decode tnewClass
    bcParent <- tryRight $ decode tbcParent
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- tryReadIORef batchHandler
    newBatch <- scriptIO $ BatchAST.addClass newClass bcParent libID projectID batch
    tryWriteIORef batchHandler newBatch
    return AddClass.Result


addFunction :: IORef Batch -> AddFunction.Args -> Script AddFunction.Result
addFunction batchHandler (AddFunction.Args tnewFunction tbcParent tlibID tprojectID) = do
    scriptIO $ loggerIO info "called addFunction"
    newFunction <- tryRight $ decode tnewFunction
    bcParent    <- tryRight $ decode tbcParent
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- tryReadIORef batchHandler
    newBatch <- scriptIO $ BatchAST.addFunction newFunction bcParent libID projectID batch
    tryWriteIORef batchHandler newBatch
    return AddFunction.Result


remove :: IORef Batch -> Remove.Args -> Script Remove.Result
remove batchHandler (Remove.Args tbc tlibID tprojectID) = do
    scriptIO $ loggerIO info "called remove"
    bc  <- tryRight $ decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- tryReadIORef batchHandler
    newBatch <- scriptIO $ BatchAST.remove bc libID projectID batch
    tryWriteIORef batchHandler newBatch
    return Remove.Result


updateModuleCls :: IORef Batch -> UpdateModuleCls.Args -> Script UpdateModuleCls.Result
updateModuleCls batchHandler (UpdateModuleCls.Args tcls tbc tlibID tprojectID) = do
    scriptIO $ loggerIO info "called updateModuleCls"
    cls <- tryRight $ decode tcls
    bc  <- tryRight $ decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- tryReadIORef batchHandler
    newBatch <- scriptIO $ BatchAST.updateModuleCls cls bc libID projectID batch
    tryWriteIORef batchHandler newBatch
    return UpdateModuleCls.Result


updateModuleImports :: IORef Batch -> UpdateModuleImports.Args -> Script UpdateModuleImports.Result
updateModuleImports batchHandler (UpdateModuleImports.Args timports tbc tlibID tprojectID) = do
    scriptIO $ loggerIO info "called updateModuleImports"
    imports <- tryRight $ decodeList timports
    bc      <- tryRight $ decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- tryReadIORef batchHandler
    newBatch <- scriptIO $ BatchAST.updateModuleImports imports bc libID projectID batch
    tryWriteIORef batchHandler newBatch
    return UpdateModuleImports.Result


updateModuleFields :: IORef Batch -> UpdateModuleFields.Args -> Script UpdateModuleFields.Result
updateModuleFields batchHandler (UpdateModuleFields.Args tfields tbc tlibID tprojectID) = do
    scriptIO $ loggerIO info "called updateModuleFields"
    fields <- tryRight $ decodeList tfields
    bc     <- tryRight $ decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- tryReadIORef batchHandler
    newBatch <- scriptIO $ BatchAST.updateModuleFields fields bc libID projectID batch
    tryWriteIORef batchHandler newBatch
    return UpdateModuleFields.Result


updateClassCls :: IORef Batch -> UpdateClassCls.Args -> Script UpdateClassCls.Result
updateClassCls batchHandler (UpdateClassCls.Args tcls tbc tlibID tprojectID) = do
    scriptIO $ loggerIO info "called updateClassCls"
    cls <- tryRight $ decode tcls
    bc  <- tryRight $ decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- tryReadIORef batchHandler
    newBatch <- scriptIO $ BatchAST.updateClassCls cls bc libID projectID batch
    tryWriteIORef batchHandler newBatch
    return UpdateClassCls.Result


updateClassFields :: IORef Batch -> UpdateClassFields.Args -> Script UpdateClassFields.Result
updateClassFields batchHandler (UpdateClassFields.Args tfields tbc tlibID tprojectID) = do
    scriptIO $ loggerIO info "called updateClassFields"
    fields <- tryRight $ decodeList tfields
    bc     <- tryRight $ decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- tryReadIORef batchHandler
    newBatch <- scriptIO $ BatchAST.updateClassFields fields bc libID projectID batch
    tryWriteIORef batchHandler newBatch
    return UpdateClassFields.Result


updateFunctionName :: IORef Batch -> UpdateFunctionName.Args -> Script UpdateFunctionName.Result
updateFunctionName batchHandler (UpdateFunctionName.Args tname tbc tlibID tprojectID) = do
    scriptIO $ loggerIO info "called updateFunctionName"
    bc <- tryRight $ decode tbc
    let name      = decodeP tname
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- tryReadIORef batchHandler
    newBatch <- scriptIO $ BatchAST.updateFunctionName name bc libID projectID batch
    tryWriteIORef batchHandler newBatch
    return UpdateFunctionName.Result


updateFunctionPath :: IORef Batch -> UpdateFunctionPath.Args -> Script UpdateFunctionPath.Result
updateFunctionPath batchHandler (UpdateFunctionPath.Args tpath tbc tlibID tprojectID) = do
    scriptIO $ loggerIO info "called updateFunctionPath"
    bc <- tryRight $ decode tbc
    let path      = decodeListP tpath
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- tryReadIORef batchHandler
    newBatch <- scriptIO $ BatchAST.updateFunctionPath path bc libID projectID batch
    tryWriteIORef batchHandler newBatch
    return UpdateFunctionPath.Result


updateFunctionInputs :: IORef Batch -> UpdateFunctionInputs.Args -> Script UpdateFunctionInputs.Result
updateFunctionInputs batchHandler (UpdateFunctionInputs.Args tinputs tbc tlibID tprojectID) = do
    scriptIO $ loggerIO info "called updateFunctionInputs"
    inputs <- tryRight $ decodeList tinputs
    bc     <- tryRight $ decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- tryReadIORef batchHandler
    newBatch <- scriptIO $ BatchAST.updateFunctionInputs inputs bc libID projectID batch
    tryWriteIORef batchHandler newBatch
    return UpdateFunctionInputs.Result


updateFunctionOutput :: IORef Batch -> UpdateFunctionOutput.Args -> Script UpdateFunctionOutput.Result
updateFunctionOutput batchHandler (UpdateFunctionOutput.Args toutput tbc tlibID tprojectID) = do
    scriptIO $ loggerIO info "called updateFunctionOutput"
    output <- tryRight $ decode toutput
    bc     <- tryRight $ decode tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    batch <- tryReadIORef batchHandler
    newBatch <- scriptIO $ BatchAST.updateFunctionOutput output bc libID projectID batch
    tryWriteIORef batchHandler newBatch
    return UpdateFunctionOutput.Result
