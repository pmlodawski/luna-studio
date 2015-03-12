---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Flowbox.ProjectManager.RPC.Handler.AST where

import qualified Flowbox.Batch.Handler.AST                                                            as BatchAST
import qualified Flowbox.Batch.Handler.Common                                                         as Batch
import           Flowbox.Bus.Data.Message                                                             (Message)
import           Flowbox.Bus.Data.Topic                                                               (Topic)
import           Flowbox.Bus.RPC.RPC                                                                  (RPC)
import           Flowbox.Data.Convert
import           Flowbox.Prelude                                                                      hiding (Context, cons)
import           Flowbox.ProjectManager.Context                                                       (Context)
import           Flowbox.ProjectManager.RPC.Handler.Graph                                             (fun, makeMsgArr) 
import qualified Flowbox.ProjectManager.RPC.Topic                                                     as Topic
import           Flowbox.System.Log.Logger
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Code.Get.Request                  as CodeGet
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Code.Get.Status                   as CodeGet
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Code.Set.Request                  as CodeSet
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Code.Set.Update                   as CodeSet
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Add.Request                  as AddData
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Add.Update                   as AddData
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Classes.Request       as ModifyDataClasses
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Classes.Update        as ModifyDataClasses
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Cls.Request           as ModifyDataCls
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Cls.Update            as ModifyDataCls
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Cons.Request          as ModifyDataCons
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Cons.Update           as ModifyDataCons
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Methods.Request       as ModifyDataMethods
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Data.Modify.Methods.Update        as ModifyDataMethods
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Add.Request              as AddFunction
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Add.Update               as AddFunction
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Inputs.Request    as ModifyFunctionInputs
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Inputs.Update     as ModifyFunctionInputs
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Name.Request      as ModifyFunctionName
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Name.Update       as ModifyFunctionName
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Output.Request    as ModifyFunctionOutput
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Output.Update     as ModifyFunctionOutput
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Path.Request      as ModifyFunctionPath
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Function.Modify.Path.Update       as ModifyFunctionPath
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Get.Request                       as Definitions
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Get.Status                        as Definitions
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Add.Request                as AddModule
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Add.Update                 as AddModule
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Modify.Cls.Request         as ModifyModuleCls
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Modify.Cls.Update          as ModifyModuleCls
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Modify.Fields.Request      as ModifyModuleFields
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Modify.Fields.Update       as ModifyModuleFields
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Modify.Imports.Request     as ModifyModuleImports
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Modify.Imports.Update      as ModifyModuleImports
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Modify.TypeAliases.Request as ModifyModuleTypeAliases
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Modify.TypeAliases.Update  as ModifyModuleTypeAliases
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Modify.TypeDefs.Request    as ModifyModuleTypeDefs
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Module.Modify.TypeDefs.Update     as ModifyModuleTypeDefs
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Remove.Request                    as Remove
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Remove.Update                     as Remove
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Resolve.Request                   as ResolveDefinition
import qualified Generated.Proto.ProjectManager.Project.Library.AST.Resolve.Status                    as ResolveDefinition
import qualified Generated.Proto.Urm.URM.Register.Request                                             as Register
import qualified Luna.DEP.AST.Control.Crumb                                                           as Crumb
import qualified Luna.DEP.AST.Expr                                                                    as Expr
import qualified Luna.DEP.AST.Module                                                                  as Module
import qualified Luna.DEP.AST.Type                                                                    as Type
import           Luna.DEP.Data.Serialize.Proto.Conversion.Crumb                                       ()
import           Luna.DEP.Data.Serialize.Proto.Conversion.Expr                                        ()
import           Luna.DEP.Data.Serialize.Proto.Conversion.Focus                                       ()
import           Luna.DEP.Data.Serialize.Proto.Conversion.Module                                      ()
import           Luna.DEP.Data.Serialize.Proto.Conversion.Name                                        ()



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


moduleAdd :: AddModule.Request -> Maybe Topic -> RPC Context IO ([AddModule.Update], [Message])
moduleAdd request@(AddModule.Request tnewModule tbcParent tlibID tprojectID _) undoTopic = do
    newModule <- decodeE tnewModule
    bcParent  <- decodeE tbcParent
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    addedModule <- BatchAST.addModule newModule bcParent libID projectID
    let newBC = bcParent ++ [Crumb.Module $ addedModule ^. Module.cls . Type.name]
    let newID = addedModule ^. Module.id
    updateNo <- Batch.getUpdateNo
    return ( [AddModule.Update request (encode addedModule) (encode newBC) updateNo]
           , makeMsgArr (Register.Request
                            (fun Topic.projectLibraryAstRemoveRequest $ Remove.Request (encode newBC) tlibID tprojectID (encodeP newID))
                            (fun Topic.projectLibraryAstModuleAddRequest request)
                            tprojectID
                        ) undoTopic
           )


dataAdd :: AddData.Request -> Maybe Topic -> RPC Context IO ([AddData.Update], [Message])
dataAdd request@(AddData.Request tnewData tbcParent tlibID tprojectID _) undoTopic = do
    newData  <- decodeE tnewData
    bcParent <- decodeE tbcParent
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    addedData <- BatchAST.addClass newData bcParent libID projectID
    let newBC = bcParent ++ [Crumb.Class $ addedData ^. Expr.cls . Type.name]
    let newID = addedData ^. Expr.id
    updateNo <- Batch.getUpdateNo
    return ( [AddData.Update request (encode addedData) (encode newBC) updateNo]
           , makeMsgArr (Register.Request
                            (fun Topic.projectLibraryAstRemoveRequest $ Remove.Request (encode newBC) tlibID tprojectID (encodeP newID))
                            (fun Topic.projectLibraryAstDataAddRequest request)
                            tprojectID
                        ) undoTopic
           )


functionAdd :: AddFunction.Request -> Maybe Topic -> RPC Context IO ([AddFunction.Update], [Message])
functionAdd request@(AddFunction.Request tnewFunction tbcParent tlibID tprojectID _) undoTopic = do
    newFunction <- decodeE tnewFunction
    bcParent    <- decodeE tbcParent
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    addedFunction <- BatchAST.addFunction newFunction bcParent libID projectID
    let newBC = bcParent ++ [Crumb.Function (addedFunction ^?! Expr.fname) (addedFunction ^. Expr.path)]
    let newID = addedFunction ^. Expr.id
    updateNo <- Batch.getUpdateNo
    return ( [AddFunction.Update request (encode addedFunction) (encode newBC) updateNo]
           , makeMsgArr (Register.Request
                            (fun Topic.projectLibraryAstRemoveRequest $ Remove.Request (encode newBC) tlibID tprojectID (encodeP newID))
                            (fun Topic.projectLibraryAstFunctionAddRequest request)
                            tprojectID
                        ) undoTopic
           )



remove :: Remove.Request -> Maybe Topic -> RPC Context IO ([Remove.Update], [Message])
remove request@(Remove.Request tbc tlibID tprojectID _) _ = do
    bc  <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchAST.remove bc libID projectID
    updateNo <- Batch.getUpdateNo
    return ([Remove.Update request updateNo], [])


resolve :: ResolveDefinition.Request -> RPC Context IO ResolveDefinition.Status
resolve request@(ResolveDefinition.Request tname tbc tlibID tprojectID _) = do
    bc  <- decodeE tbc
    let name      = decodeP tname
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    results <- BatchAST.resolveDefinition name bc libID projectID
    return $ ResolveDefinition.Status request (encode results)


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
    imports <- decodeE timports
    bc      <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchAST.updateModuleImports imports bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ ModifyModuleImports.Update request updateNo


moduleTypeAliasesModify :: ModifyModuleTypeAliases.Request -> RPC Context IO ModifyModuleTypeAliases.Update
moduleTypeAliasesModify request@(ModifyModuleTypeAliases.Request tfields tbc tlibID tprojectID _) = do
    fields <- decodeE tfields
    bc     <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchAST.updateModuleFields fields bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ ModifyModuleTypeAliases.Update request updateNo


moduleTypeDefsModify :: ModifyModuleTypeDefs.Request -> RPC Context IO ModifyModuleTypeDefs.Update
moduleTypeDefsModify request@(ModifyModuleTypeDefs.Request tfields tbc tlibID tprojectID _) = do
    fields <- decodeE tfields
    bc     <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchAST.updateModuleFields fields bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ ModifyModuleTypeDefs.Update request updateNo


moduleFieldsModify :: ModifyModuleFields.Request -> RPC Context IO ModifyModuleFields.Update
moduleFieldsModify request@(ModifyModuleFields.Request tfields tbc tlibID tprojectID _) = do
    fields <- decodeE tfields
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
    cons <- decodeE tcons
    bc   <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchAST.updateDataCons cons bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ ModifyDataCons.Update request updateNo


dataClassesModify :: ModifyDataClasses.Request -> RPC Context IO ModifyDataClasses.Update
dataClassesModify request@(ModifyDataClasses.Request tclasses tbc tlibID tprojectID _) = do
    classes <- decodeE tclasses
    bc      <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchAST.updateDataClasses classes bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ ModifyDataClasses.Update request updateNo


dataMethodsModify :: ModifyDataMethods.Request -> RPC Context IO ModifyDataMethods.Update
dataMethodsModify request@(ModifyDataMethods.Request tmethods tbc tlibID tprojectID _) = do
    methods <- decodeE tmethods
    bc      <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchAST.updateDataMethods methods bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ ModifyDataMethods.Update request updateNo


functionNameModify :: ModifyFunctionName.Request -> Maybe Topic -> RPC Context IO ([ModifyFunctionName.Update], [Message])
functionNameModify request@(ModifyFunctionName.Request tname tbc tlibID tprojectID astID) undoTopic = do
    bc <- decodeE tbc
    name <- decodeE tname
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    focus <- Batch.getFunctionFocus bc libID projectID
    let tnewBC    = encode $ init bc ++ [Crumb.Function name $ focus ^. Expr.path]
    BatchAST.updateFunctionName name bc libID projectID
    updateNo <- Batch.getUpdateNo
    return ( [ModifyFunctionName.Update request updateNo]
           , makeMsgArr ( Register.Request
                              (fun Topic.projectLibraryAstFunctionModifyNameRequest $ ModifyFunctionName.Request (encode $ focus ^?! Expr.fname) tnewBC tlibID tprojectID astID)
                              (fun Topic.projectLibraryAstFunctionModifyNameRequest $ request)
                              tprojectID
                        ) undoTopic
           )
                        


functionPathModify :: ModifyFunctionPath.Request -> RPC Context IO ModifyFunctionPath.Update
functionPathModify request@(ModifyFunctionPath.Request tpath tbc tlibID tprojectID _) = do
    bc <- decodeE tbc
    let path      = decodeP tpath
        libID     = decodeP tlibID
        projectID = decodeP tprojectID
    BatchAST.updateFunctionPath path bc libID projectID
    updateNo <- Batch.getUpdateNo
    return $ ModifyFunctionPath.Update request updateNo


functionInputsModify :: ModifyFunctionInputs.Request -> RPC Context IO ModifyFunctionInputs.Update
functionInputsModify request@(ModifyFunctionInputs.Request tinputs tbc tlibID tprojectID _) = do
    inputs <- decodeE tinputs
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


codeGet :: CodeGet.Request -> RPC Context IO CodeGet.Status
codeGet request@(CodeGet.Request tbc tlibID tprojectID _) = do
    bc <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
    code <- Batch.getCode bc libID projectID
    foldr seq (return ()) code
    return $ CodeGet.Status request $ encodeP code


codeSet :: CodeSet.Request -> RPC Context IO CodeSet.Update
codeSet request@(CodeSet.Request tcode tbc tlibID tprojectID _) = do
    bc <- decodeE tbc
    let libID     = decodeP tlibID
        projectID = decodeP tprojectID
        code      = decodeP tcode
    Batch.setCode code bc libID projectID
    return $ CodeSet.Update request
