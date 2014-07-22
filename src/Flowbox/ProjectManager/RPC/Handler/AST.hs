---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.ProjectManager.RPC.Handler.AST where

import qualified Flowbox.Batch.Handler.AST                                                         as BatchAST
import           Flowbox.Bus.RPC.RPC                                                               (RPC)
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
import qualified Flowbox.ProjectManager.Context                                                    as Context
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



logger :: LoggerIO
logger = getLoggerIO "Flowbox.ProjectManager.RPC.Handler.AST"

-------- public api -------------------------------------------------

get :: ContextRef -> Definitions.Request -> RPC IO Definitions.Status
get ctxRef request@(Definitions.Request mtmaxDepth tbc tlibID tprojectID _) = do
    bc  <- decodeE tbc
    let mmaxDepth = fmap decodeP mtmaxDepth
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    focus <- Context.run ctxRef $ BatchAST.definitions mmaxDepth bc libID projectID
    return $ Definitions.Status request $ encode focus


moduleAdd :: ContextRef -> AddModule.Request -> RPC IO AddModule.Update
moduleAdd ctxRef request@(AddModule.Request tnewModule tbcParent tlibID tprojectID _) = do
    newModule <- decodeE tnewModule
    bcParent  <- decodeE tbcParent
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    addedModule <- Context.run ctxRef $ BatchAST.addModule newModule bcParent libID projectID
    let newBC = bcParent ++ [Crumb.ModuleCrumb $ addedModule ^. Module.cls . Type.name]
    return $ AddModule.Update request (encode addedModule) (encode newBC)


dataAdd :: ContextRef -> AddData.Request -> RPC IO AddData.Update
dataAdd ctxRef request@(AddData.Request tnewData tbcParent tlibID tprojectID _) = do
    newData  <- decodeE tnewData
    bcParent <- decodeE tbcParent
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    addedData <- Context.run ctxRef $ BatchAST.addClass newData bcParent libID projectID
    let newBC = bcParent ++ [Crumb.ClassCrumb $ addedData ^. Expr.cls . Type.name]
    return $ AddData.Update request (encode addedData) (encode newBC)


functionAdd :: ContextRef -> AddFunction.Request -> RPC IO AddFunction.Update
functionAdd ctxRef request@(AddFunction.Request tnewFunction tbcParent tlibID tprojectID _) = do
    newFunction <- decodeE tnewFunction
    bcParent    <- decodeE tbcParent
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    addedFunction <- Context.run ctxRef $ BatchAST.addFunction newFunction bcParent libID projectID
    let newBC = bcParent ++ [Crumb.FunctionCrumb (addedFunction ^. Expr.name) (addedFunction ^. Expr.path)]
    return $ AddFunction.Update request (encode addedFunction) (encode newBC)



remove :: ContextRef -> Remove.Request -> RPC IO Remove.Update
remove ctxRef request@(Remove.Request tbc tlibID tprojectID _) = do
    bc  <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    Context.run ctxRef $ BatchAST.remove bc libID projectID
    return $ Remove.Update request


resolve :: ContextRef -> ResolveDefinition.Request -> RPC IO ResolveDefinition.Status
resolve ctxRef request@(ResolveDefinition.Request tname tbc tlibID tprojectID) = do
    bc  <- decodeE tbc
    let name      = decodeP tname
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    results <- Context.run ctxRef $ BatchAST.resolveDefinition name bc libID projectID
    return $ ResolveDefinition.Status request (encodeList results)


moduleClsModify :: ContextRef -> ModifyModuleCls.Request -> RPC IO ModifyModuleCls.Update
moduleClsModify ctxRef request@(ModifyModuleCls.Request tcls tbc tlibID tprojectID _) = do
    cls <- decodeE tcls
    bc  <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    Context.run ctxRef $ BatchAST.updateModuleCls cls bc libID projectID
    return $ ModifyModuleCls.Update request


moduleImportsModify :: ContextRef -> ModifyModuleImports.Request -> RPC IO ModifyModuleImports.Update
moduleImportsModify ctxRef request@(ModifyModuleImports.Request timports tbc tlibID tprojectID _) = do
    imports <- decodeListE timports
    bc      <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    Context.run ctxRef $ BatchAST.updateModuleImports imports bc libID projectID
    return $ ModifyModuleImports.Update request


moduleFieldsModify :: ContextRef -> ModifyModuleFields.Request -> RPC IO ModifyModuleFields.Update
moduleFieldsModify ctxRef request@(ModifyModuleFields.Request tfields tbc tlibID tprojectID _) = do
    fields <- decodeListE tfields
    bc     <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    Context.run ctxRef $ BatchAST.updateModuleFields fields bc libID projectID
    return $ ModifyModuleFields.Update request


dataClsModify :: ContextRef -> ModifyDataCls.Request -> RPC IO ModifyDataCls.Update
dataClsModify ctxRef request@(ModifyDataCls.Request tcls tbc tlibID tprojectID _) = do
    cls <- decodeE tcls
    bc  <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    Context.run ctxRef $ BatchAST.updateDataCls cls bc libID projectID
    return $ ModifyDataCls.Update request


dataConsModify :: ContextRef -> ModifyDataCons.Request -> RPC IO ModifyDataCons.Update
dataConsModify ctxRef request@(ModifyDataCons.Request tcons tbc tlibID tprojectID _) = do
    cons <- decodeListE tcons
    bc   <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    Context.run ctxRef $ BatchAST.updateDataCons cons bc libID projectID
    return $ ModifyDataCons.Update request


dataClassesModify :: ContextRef -> ModifyDataClasses.Request -> RPC IO ModifyDataClasses.Update
dataClassesModify ctxRef request@(ModifyDataClasses.Request tclasses tbc tlibID tprojectID _) = do
    classes <- decodeListE tclasses
    bc      <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    Context.run ctxRef $ BatchAST.updateDataClasses classes bc libID projectID
    return $ ModifyDataClasses.Update request


dataMethodsModify :: ContextRef -> ModifyDataMethods.Request -> RPC IO ModifyDataMethods.Update
dataMethodsModify ctxRef request@(ModifyDataMethods.Request tmethods tbc tlibID tprojectID _) = do
    methods <- decodeListE tmethods
    bc      <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    Context.run ctxRef $ BatchAST.updateDataMethods methods bc libID projectID
    return $ ModifyDataMethods.Update request


functionNameModify :: ContextRef -> ModifyFunctionName.Request -> RPC IO ModifyFunctionName.Update
functionNameModify ctxRef request@(ModifyFunctionName.Request tname tbc tlibID tprojectID _) = do
    bc <- decodeE tbc
    let name      = decodeP tname
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    Context.run ctxRef $  BatchAST.updateFunctionName name bc libID projectID
    return $ ModifyFunctionName.Update request


functionPathModify :: ContextRef -> ModifyFunctionPath.Request -> RPC IO ModifyFunctionPath.Update
functionPathModify ctxRef request@(ModifyFunctionPath.Request tpath tbc tlibID tprojectID _) = do
    bc <- decodeE tbc
    let path      = decodeListP tpath
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    Context.run ctxRef $ BatchAST.updateFunctionPath path bc libID projectID
    return $ ModifyFunctionPath.Update request


functionInputsModify :: ContextRef -> ModifyFunctionInputs.Request -> RPC IO ModifyFunctionInputs.Update
functionInputsModify ctxRef request@(ModifyFunctionInputs.Request tinputs tbc tlibID tprojectID _) = do
    inputs <- decodeListE tinputs
    bc     <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    Context.run ctxRef $ BatchAST.updateFunctionInputs inputs bc libID projectID
    return $ ModifyFunctionInputs.Update request


functionOutputModify :: ContextRef -> ModifyFunctionOutput.Request -> RPC IO ModifyFunctionOutput.Update
functionOutputModify ctxRef request@(ModifyFunctionOutput.Request toutput tbc tlibID tprojectID _) = do
    output <- decodeE toutput
    bc     <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    Context.run ctxRef $ BatchAST.updateFunctionOutput output bc libID projectID
    return $ ModifyFunctionOutput.Update request
