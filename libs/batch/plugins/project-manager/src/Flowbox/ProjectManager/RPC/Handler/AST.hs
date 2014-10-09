---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Flowbox.ProjectManager.RPC.Handler.AST where

import qualified Flowbox.Batch.Handler.AST                                                         as BatchAST
import qualified Flowbox.Batch.Handler.Common                                                      as Batch
import           Flowbox.Bus.RPC.RPC                                                               (RPC)
import           Flowbox.Prelude                                                                   hiding (Context, cons)
import           Flowbox.ProjectManager.Context                                                    (Context)
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
import qualified Luna.AST.Control.Crumb                                                            as Crumb
import qualified Luna.AST.Expr                                                                     as Expr
import qualified Luna.AST.Module                                                                   as Module
import qualified Luna.AST.Type                                                                     as Type
import           Luna.Data.Serialize.Proto.Conversion.Crumb                                        ()
import           Luna.Data.Serialize.Proto.Conversion.Expr                                         ()
import           Luna.Data.Serialize.Proto.Conversion.Focus                                        ()
import           Luna.Data.Serialize.Proto.Conversion.Module                                       ()



logger :: LoggerIO
logger = getLoggerIO $(moduleName)

-------- public api -------------------------------------------------

get :: Definitions.Request -> RPC Context IO Definitions.Status
get request@(Definitions.Request mtmaxDepth tbc tlibID tprojectID _) = do
    bc  <- decodeE tbc
    let mmaxDepth = fmap decodeP mtmaxDepth
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    focus <- BatchAST.definitions mmaxDepth bc libID projectID
    return $ Definitions.Status request $ encode focus


moduleAdd :: AddModule.Request -> RPC Context IO AddModule.Update
moduleAdd request@(AddModule.Request tnewModule tbcParent tlibID tprojectID _) = do
    newModule <- decodeE tnewModule
    bcParent  <- decodeE tbcParent
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    addedModule <- BatchAST.addModule newModule bcParent libID projectID
    let newBC = bcParent ++ [Crumb.Module $ addedModule ^. Module.cls . Type.name]
    updateNo <- Batch.getUpdateNo
    return $ AddModule.Update request (encode addedModule) (encode newBC) updateNo


dataAdd :: AddData.Request -> RPC Context IO AddData.Update
dataAdd request@(AddData.Request tnewData tbcParent tlibID tprojectID _) = do
    newData  <- decodeE tnewData
    bcParent <- decodeE tbcParent
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    addedData <- BatchAST.addClass newData bcParent libID projectID
    let newBC = bcParent ++ [Crumb.Class $ addedData ^. Expr.cls . Type.name]
    updateNo <- Batch.getUpdateNo
    return $ AddData.Update request (encode addedData) (encode newBC) updateNo


functionAdd :: AddFunction.Request -> RPC Context IO AddFunction.Update
functionAdd request@(AddFunction.Request tnewFunction tbcParent tlibID tprojectID _) = do
    newFunction <- decodeE tnewFunction
    bcParent    <- decodeE tbcParent
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    addedFunction <- BatchAST.addFunction newFunction bcParent libID projectID
    let newBC = bcParent ++ [Crumb.Function (addedFunction ^. Expr.name) (addedFunction ^. Expr.path)]
    updateNo <- Batch.getUpdateNo
    return $ AddFunction.Update request (encode addedFunction) (encode newBC) updateNo



remove :: Remove.Request -> RPC Context IO Remove.Update
remove request@(Remove.Request tbc tlibID tprojectID _) = do
    bc  <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchAST.remove bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ Remove.Update request updateNo


resolve :: ResolveDefinition.Request -> RPC Context IO ResolveDefinition.Status
resolve request@(ResolveDefinition.Request tname tbc tlibID tprojectID _) = do
    bc  <- decodeE tbc
    let name      = decodeP tname
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    results <- BatchAST.resolveDefinition name bc libID projectID
    return $ ResolveDefinition.Status request (encodeList results)


moduleClsModify :: ModifyModuleCls.Request -> RPC Context IO ModifyModuleCls.Update
moduleClsModify request@(ModifyModuleCls.Request tcls tbc tlibID tprojectID _) = do
    cls <- decodeE tcls
    bc  <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchAST.updateModuleCls cls bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ ModifyModuleCls.Update request updateNo


moduleImportsModify :: ModifyModuleImports.Request -> RPC Context IO ModifyModuleImports.Update
moduleImportsModify request@(ModifyModuleImports.Request timports tbc tlibID tprojectID _) = do
    imports <- decodeListE timports
    bc      <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchAST.updateModuleImports imports bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ ModifyModuleImports.Update request updateNo


moduleFieldsModify :: ModifyModuleFields.Request -> RPC Context IO ModifyModuleFields.Update
moduleFieldsModify request@(ModifyModuleFields.Request tfields tbc tlibID tprojectID _) = do
    fields <- decodeListE tfields
    bc     <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchAST.updateModuleFields fields bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ ModifyModuleFields.Update request updateNo


dataClsModify :: ModifyDataCls.Request -> RPC Context IO ModifyDataCls.Update
dataClsModify request@(ModifyDataCls.Request tcls tbc tlibID tprojectID _) = do
    cls <- decodeE tcls
    bc  <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchAST.updateDataCls cls bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ ModifyDataCls.Update request updateNo


dataConsModify :: ModifyDataCons.Request -> RPC Context IO ModifyDataCons.Update
dataConsModify request@(ModifyDataCons.Request tcons tbc tlibID tprojectID _) = do
    cons <- decodeListE tcons
    bc   <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchAST.updateDataCons cons bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ ModifyDataCons.Update request updateNo


dataClassesModify :: ModifyDataClasses.Request -> RPC Context IO ModifyDataClasses.Update
dataClassesModify request@(ModifyDataClasses.Request tclasses tbc tlibID tprojectID _) = do
    classes <- decodeListE tclasses
    bc      <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchAST.updateDataClasses classes bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ ModifyDataClasses.Update request updateNo


dataMethodsModify :: ModifyDataMethods.Request -> RPC Context IO ModifyDataMethods.Update
dataMethodsModify request@(ModifyDataMethods.Request tmethods tbc tlibID tprojectID _) = do
    methods <- decodeListE tmethods
    bc      <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchAST.updateDataMethods methods bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ ModifyDataMethods.Update request updateNo


functionNameModify :: ModifyFunctionName.Request -> RPC Context IO ModifyFunctionName.Update
functionNameModify request@(ModifyFunctionName.Request tname tbc tlibID tprojectID _) = do
    bc <- decodeE tbc
    let name      = decodeP tname
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchAST.updateFunctionName name bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ ModifyFunctionName.Update request updateNo


functionPathModify :: ModifyFunctionPath.Request -> RPC Context IO ModifyFunctionPath.Update
functionPathModify request@(ModifyFunctionPath.Request tpath tbc tlibID tprojectID _) = do
    bc <- decodeE tbc
    let path      = decodeListP tpath
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchAST.updateFunctionPath path bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ ModifyFunctionPath.Update request updateNo


functionInputsModify :: ModifyFunctionInputs.Request -> RPC Context IO ModifyFunctionInputs.Update
functionInputsModify request@(ModifyFunctionInputs.Request tinputs tbc tlibID tprojectID _) = do
    inputs <- decodeListE tinputs
    bc     <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchAST.updateFunctionInputs inputs bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ ModifyFunctionInputs.Update request updateNo


functionOutputModify :: ModifyFunctionOutput.Request -> RPC Context IO ModifyFunctionOutput.Update
functionOutputModify request@(ModifyFunctionOutput.Request toutput tbc tlibID tprojectID _) = do
    output <- decodeE toutput
    bc     <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchAST.updateFunctionOutput output bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ ModifyFunctionOutput.Update request updateNo
